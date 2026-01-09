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
            cls := "hidden lg:flex items-center justify-center mb-8",
            img(
              cls := "w-8 mr-2",
              src := "./assets/logo.svg"
            ),
            a(
              href := "/",
              cls := "text-xl font-bold text-slate-100",
              "Nostr Web Army Knife"
            )
          ),
          div(
            cls := "flex-1",
            renderResult(store)
          ),
          // signing preferences
          div(
            cls := "lg:mt-6 pt-6 border-t border-slate-800",
            renderNip07Signer(store)
          ),
          // links at bottom
          div(
            cls := "flex gap-2 justify-center flex-wrap items-center lg:mt-6 pt-6 border-t border-slate-800 text-sm text-slate-400",
            span("fork of "),
            a(
              href := "https://nwak.nostr.technology/",
              target := "_blank",
              cls := "text-primary-400 hover:text-primary-300 transition-colors font-medium",
              "fiatjaf/nwak"
            ),
            span(" with a sexier look — "),
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
      cls := "flex flex-col gap-4 pt-4",

      // Toolbar for utility actions
      div(
        Styles.toolbar,
        store.input.map {
          case "" => div("")
          case _ =>
            button(
              Styles.toolbarButton,
              "Clear",
              onClick --> (_.foreach(_ => store.input.set("")))
            )
        },
        store.result.map {
          case Right(_: Event) =>
            button(
              Styles.toolbarButton,
              "Format",
              onClick --> (_.foreach(_ =>
                store.input.update(original =>
                  parse(original).toOption
                    .map(_.printWith(jsonPrinter))
                    .getOrElse(original)
                )
              ))
            )
          case _ => div("")
        }
      ),

      // Action cards grid
      div(
        cls := "flex flex-wrap gap-3 justify-center",

        // Fetch card (conditional)
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
                      case None =>
                        store.input.set(
                          s"Tried all the given relay hints, but event ${evp.id} was not found."
                        )

                  val fetchOrUnit = fetchIsInProgress.get.flatMap {
                    case true => IO.unit
                    case false =>
                      fetchIsInProgress.set(true)
                        *> tryFetchFromEachOrNone.flatMap(updateInput)
                        *> fetchIsInProgress.set(false)
                  }
                  val buttonLabel = fetchIsInProgress.map {
                    case true  => "Fetching..."
                    case false => "Fetch Event"
                  }

                  actionCard(
                    "Fetch from Relay",
                    "Retrieve the event from the provided relay hints",
                    button(
                      Styles.button,
                      buttonLabel,
                      onClick --> (_.foreach(_ => fetchOrUnit)),
                      disabled <-- fetchIsInProgress
                    )
                  )
              }
            )
          case _ => None
        },

        actionCard(
          "Generate New",
          "",
          button(
            Styles.buttonWithIcon,
            img(cls := "w-5 h-5", src := "./assets/braces.svg"),
            span("Generate Event"),
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
            Styles.buttonWithIcon,
            img(cls := "w-5 h-5", src := "./assets/key.svg"),
            span("Generate Keypair"),
            onClick --> (_.foreach(_ =>
              store.input.set(
                NIP19.encode(PrivateKey(randomBytes32()))
              )
            ))
          )
        )
      )
    )

  def renderInput(store: Store): Resource[IO, HtmlDivElement[IO]] =
    div(
      cls := "w-full",
      textArea.withSelf { self =>
        (
          cls := "w-full p-4 lg:p-5 min-h-[280px] lg:min-h-[370px] lg:min-w-[500px] font-mono text-sm rounded-xl bg-slate-800/50 border border-slate-700 text-slate-100 placeholder:text-slate-500 focus:outline-none focus:ring-2 focus:ring-primary-500/50 focus:border-primary-500 transition-all",
          spellCheck := false,
          placeholder := "Paste something nostric (event JSON, nprofile, npub, nevent etc or hex key or id)",
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
        case Right(bytes: ByteVector32) => render32Bytes(store, bytes, window)
        case Right(event: Event)        => renderEvent(store, event, window)
        case Right(pp: ProfilePointer)  => renderProfilePointer(store, pp, None, window)
        case Right(evp: EventPointer)   => renderEventPointer(store, evp, window)
        case Right(sk: PrivateKey) =>
          renderProfilePointer(
            store,
            ProfilePointer(pubkey = sk.publicKey.xonly),
            Some(sk),
            window
          )
        case Right(addr: AddressPointer) => renderAddressPointer(store, addr, window)
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
              signerProfileCard(
                signerType = "NIP-07",
                pubkeyHex = signer.flatMap(_.publicKeyHex.toResource),
                switchButtonLabel = "Switch",
                onSwitch = store.nip07signer.set(NIP07.mkDebuggingSigner())
                  *> useNip07.set(false),
                iconSrc = "./assets/puzzle.svg",
                nip07Available = true,
                onSwitchToNip07 = IO.unit,
                window = window
              )
            case (true, false, signer) =>
              signerProfileCard(
                signerType = "Debugging",
                pubkeyHex = signer.flatMap(_.publicKeyHex.toResource),
                switchButtonLabel = "Switch",
                onSwitch = store.nip07signer.set(NIP07.mkDebuggingSigner())
                  *> useNip07.set(false),
                iconSrc = "./assets/bug.svg",
                nip07Available = true,
                onSwitchToNip07 = NIP07
                  .mkSigner(window)
                  .evalTap(_.publicKey.timeout(1.seconds))
                  .attempt
                  .map {
                    case Left(_) => false
                    case Right(_) => true
                  }
                  .use(switchWasSuccessful =>
                    if switchWasSuccessful then
                      store.nip07signer.set(NIP07.mkSigner(window))
                        *> useNip07.set(true)
                    else
                      IO.unit
                  ),
                window = window
              )
            case (_, _, signer) =>
              signerProfileCard(
                signerType = "Debugging",
                pubkeyHex = signer.flatMap(_.publicKeyHex.toResource),
                switchButtonLabel = "",
                onSwitch = IO.unit,
                iconSrc = "./assets/bug.svg",
                nip07Available = false,
                window = window
              )
          }
        )
      yield html
    }
}
