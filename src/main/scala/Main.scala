import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.concurrent.*
import fs2.dom.{Event as _, *}
import io.circe.parser.*
import io.circe.syntax.*
import calico.*
import calico.html.io.{*, given}
import calico.syntax.*
import scoin.*
import snow.*
import calico.frp.given

import Utils.*
import Components.*
import scala.concurrent.duration.{span as _, *}

object Main extends IOWebApp {
  def render: Resource[IO, HtmlDivElement[IO]] = Store(window).flatMap {
    store =>
      div(
        cls := "grid lg:flex grid-rows-[auto_auto] lg:flex-row w-full min-h-screen",
        // sidebar
        div(
          cls := "order-2 lg:order-1 justify-self-end w-full lg:w-1/2 bg-slate-900/50 mt-6 lg:mt-0 p-6 flex flex-col border-r border-slate-800",
          h1(
            cls := "hidden lg:flex items-center justify-end mb-8",
            img(
              cls := "w-8 mr-2",
              src := "./favicon.ico"
            ),
            a(
              href := "/",
              cls := "text-xl font-bold text-slate-100",
              "nostr web army knife"
            )
          ),
          div(
            cls := "flex-1",
            renderResult(store)
          ),
          // signing preferences
          div(
            cls := "flex gap-2 justify-end flex-wrap lg:mt-6 pt-6 border-t border-slate-800 space-y-4 text-sm text-slate-400",
            renderNip07Signer(store)
          ),
          // links at bottom
          div(
            cls := "flex gap-2 justify-end flex-wrap items-center lg:mt-6 pt-6 border-t border-slate-800 text-sm text-slate-400",
            span("fork of "),
            a(
              href := "https://nwak.nostr.technology/",
              target := "_blank",
              cls := "text-primary-400 hover:text-primary-300 transition-colors font-medium",
              "fiatjaf/nwak"
            ),
            span(" with a sexy dark theme — "),
            a(
              href := "https://github.com/Unsigned-Labs/nwak",
              target := "_blank",
              cls := "hover:text-primary-400 transition-colors underline",
              "our code"
            ),
            span(" • "),
            a(
              href := "https://github.com/fiatjaf/nwak",
              target := "_blank",
              cls := "hover:text-primary-400 transition-colors underline",
              "original"
            )
          )
        ),
        // main content
        div(
          cls := "order-1 lg-order-2 justify-self-start lg:flex lg:items-center lg:justify-center w-full",
          div(
            cls := "bg-slate-900 w-full lg:w-[90%] lg:rounded-2xl shadow-2xl shadow-black/50 border border-slate-800/50 p-4 pt-6 lg:p-12",
            renderInput(store),
            renderActions(store)
          )
        )
      )
  }

  def renderActions(store: Store): Resource[IO, HtmlDivElement[IO]] =
    div(
      cls := "flex flex-wrap justify-evenly pt-4 gap-2",
      store.input.map {
        case "" => div("")
        case _ =>
          button(
            Styles.button,
            "clear",
            onClick --> (_.foreach(_ => store.input.set("")))
          )
      },
      store.result.map {
        case Right(_: Event) =>
          button(
            Styles.button,
            "format",
            onClick --> (_.foreach(_ =>
              store.input.update(original =>
                parse(original).toOption
                  .map(_.printWith(jsonPrinter))
                  .getOrElse(original)
              )
            ))
          )
        case _ => div("")
      },
      store.result.map {
        case Right(evp: EventPointer) if evp.relays.nonEmpty =>
          Some(
            SignallingRef[IO].of(false).toResource.flatMap {
              fetchIsInProgress =>

                def fetchFromRelay(rawUri: String): IO[Option[Event]] =
                  IO.fromEither(org.http4s.Uri.fromString(rawUri))
                    .toResource
                    .flatMap(Relay.mkResourceForIO(_))
                    .use { relay =>
                      relay.lookupEventById(evp.id, timeout = 30.seconds)
                    }
                    .reject { case None =>
                      new RuntimeException(
                        s"event-not-found: ${evp.id} not found at $rawUri"
                      )
                    }

                val tryFetchFromEachOrNone =
                  multiRaceAllFailOrFirstToSucceed(
                    evp.relays.map(fetchFromRelay)
                  )
                    .recover(_ => None)

                def updateInput(maybeEvent: Option[Event]): IO[Unit] =
                  maybeEvent match
                    case Some(event) =>
                      store.input.set(event.asJson.printWith(jsonPrinter))
                    // for now we will just display a failure message in the input
                    // textarea, but this should be made better
                    case None =>
                      store.input.set(
                        s"tried all the given relay hints, but event ${evp.id} was not found."
                      )

                val fetchOrUnit = fetchIsInProgress.get.flatMap {
                  case true => IO.unit
                  case false =>
                    fetchIsInProgress.set(true)
                      *> tryFetchFromEachOrNone.flatMap(updateInput)
                      *> fetchIsInProgress.set(false)
                }
                val buttonLabel = fetchIsInProgress.map {
                  case true  => "fetching ..."
                  case false => "fetch event"
                }
                button(
                  Styles.button,
                  buttonLabel,
                  onClick --> (_.foreach(_ => fetchOrUnit)),
                  disabled <-- fetchIsInProgress
                )
            }
          )
        case _ => None
      },
      button(
        Styles.button,
        "generate event",
        onClick --> (_.foreach(_ =>
          Resource
            .suspend(store.nip07signer.get)
            .use { signer =>
              for
                pubkey <- signer.publicKey
                generatedEvent <- IO(
                  Event(
                    kind = 1,
                    content = "hello world",
                    pubkey = Some(pubkey),
                    id = None
                  )
                )
                // only auto-sign the event if we are using debugging signer
                signedEvent <- signer.isDebuggingSigner.ifM(
                  ifTrue = signer.signEvent(generatedEvent),
                  ifFalse = IO(generatedEvent)
                )
              yield signedEvent
            }
            .map(_.asJson.printWith(jsonPrinter))
            .flatMap(store.input.set)
        ))
      ),
      button(
        Styles.button,
        "generate keypair",
        onClick --> (_.foreach(_ =>
          store.input.set(
            NIP19.encode(PrivateKey(randomBytes32()))
          )
        ))
      )
    )

  def renderInput(store: Store): Resource[IO, HtmlDivElement[IO]] =
    div(
      cls := "w-full",
      textArea.withSelf { self =>
        (
          cls := "w-full p-4 lg:p-5 min-h-[280px] lg:min-h-[370px] lg:min-w-[500px] font-mono text-sm rounded-xl bg-slate-800/50 border border-slate-700 text-slate-100 placeholder:text-slate-500 focus:outline-none focus:ring-2 focus:ring-primary-500/50 focus:border-primary-500 transition-all",
          spellCheck := false,
          placeholder := "paste something nostric (event JSON, nprofile, npub, nevent etc or hex key or id)",
          onInput --> (_.foreach(_ => self.value.get.flatMap(store.input.set))),
          value <-- store.input
        )
      }
    )

  def renderResult(store: Store): Resource[IO, HtmlDivElement[IO]] =
    div(
      cls := "w-full",
      store.result.map {
        case Left(msg)                  => div(msg)
        case Right(bytes: ByteVector32) => render32Bytes(store, bytes)
        case Right(event: Event)        => renderEvent(store, event)
        case Right(pp: ProfilePointer)  => renderProfilePointer(store, pp)
        case Right(evp: EventPointer)   => renderEventPointer(store, evp)
        case Right(sk: PrivateKey) =>
          renderProfilePointer(
            store,
            ProfilePointer(pubkey = sk.publicKey.xonly),
            Some(sk)
          )
        case Right(addr: AddressPointer) => renderAddressPointer(store, addr)
      }
    )

  def renderNip07Signer(store: Store): Resource[IO, HtmlDivElement[IO]] =
    (
      SignallingRef[IO].of(false).toResource,
      SignallingRef[IO].of(false).toResource
    ).flatMapN { (nip07isAvailable, useNip07) =>
      for
        _ <- NIP07.isAvailable.flatMap(nip07isAvailable.set).background
        _ <- store.nip07signer.discrete
          .evalTap(
            _.use(_.isDebuggingSigner)
              .ifM(
                ifTrue = useNip07.set(false),
                ifFalse = useNip07.set(true)
              )
          )
          .compile
          .drain
          .background
        html <- div(
          (
            nip07isAvailable: Signal[IO, Boolean],
            useNip07: Signal[IO, Boolean],
            store.nip07signer: Signal[IO, Resource[IO, NIP07Signer[IO]]]
          ).mapN {
            case (true, true, signer) =>
              div(
                span(cls := "font-bold", "using NIP07 pubkey: "),
                span(cls := "mr-4", signer.flatMap(_.publicKeyHex.toResource)),
                button(
                  "switch to debugging key",
                  Styles.buttonSmall,
                  onClick --> (_.foreach(_ =>
                    store.nip07signer.set(NIP07.mkDebuggingSigner())
                      *> useNip07.set(false)
                  ))
                )
              )
            case (true, false, signer) =>
              div(
                span(cls := "font-bold", "using debugging pubkey: "),
                span(cls := "mr-4", signer.flatMap(_.publicKeyHex.toResource)),
                button(
                  "switch to NIP-07",
                  Styles.buttonSmall,
                  onClick --> (_.foreach(_ =>
                    NIP07
                      .mkSigner(window)
                      // try to see if we have a public key yet
                      .evalTap(_.publicKey.timeout(1.seconds))
                      .attempt
                      .map {
                        // timeout was triggered
                        case Left(_) => false
                        // no error, so
                        case Right(_) => true
                      }
                      .use(switchWasSuccessful =>
                        if switchWasSuccessful then
                          // since we are this far, we can probably safely
                          // construct a new NIP07 signer and not need to guard
                          // it with a timeout like above
                          store.nip07signer.set(NIP07.mkSigner(window))
                            *> useNip07.set(true)
                        else
                          // no change in signer
                          IO.unit
                      )
                  ))
                )
              )
            case (_, _, signer) =>
              div(
                span(cls := "font-bold", "using debugging pubkey: "),
                span(signer.flatMap(_.publicKeyHex.toResource))
              )
          }
        )
      yield html
    }
}
