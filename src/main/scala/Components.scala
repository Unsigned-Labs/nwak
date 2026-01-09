import cats.data.{Store as *, *}
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
import calico.frp.given
import scodec.bits.ByteVector
import scoin.*
import snow.*

import org.http4s.syntax.literals.uri

import Utils.*

object Components {
  private def card(
      title: String,
      content: Resource[IO, HtmlDivElement[IO]]*
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      Styles.card,
      div(Styles.cardTitle, title),
      div(cls := "flex flex-col gap-2", content.toList)
    )

  private def cardWithIcon(
      title: String,
      iconSrc: String,
      content: Resource[IO, HtmlDivElement[IO]]*
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      Styles.card,
      div(
        Styles.cardTitle,
        cls := "flex items-center gap-2",
        img(cls := "w-5 h-5", src := iconSrc),
        span(title)
      ),
      div(cls := "flex flex-col gap-2", content.toList)
    )

  def truncatePubkey(hex: String, startChars: Int = 8, endChars: Int = 8): String =
    if hex.length <= startChars + endChars then hex
    else s"${hex.take(startChars)}...${hex.takeRight(endChars)}"

  def signerProfileCard(
      signerType: String,
      pubkeyHex: Resource[IO, String],
      switchButtonLabel: String,
      onSwitch: IO[Unit],
      iconSrc: String,
      nip07Available: Boolean = false,
      onSwitchToNip07: IO[Unit] = IO.unit,
      window: Window[IO]
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      Styles.card,
      cls := "flex items-center gap-4",
      div(
        Styles.iconContainer,
        img(cls := "w-6 h-6", src := iconSrc)
      ),
      div(
        cls := "flex-1 min-w-0",
        div(Styles.sectionLabel, s"$signerType Signer"),
        pubkeyHex.flatMap(hex =>
          div(
            cls := "flex flex-col gap-1",
            div(
              cls := "flex items-center gap-2",
              span(cls := "text-xs text-slate-500", "Hex:"),
              button(
                cls := "font-mono text-sm text-slate-200 hover:text-primary-400 cursor-pointer bg-transparent border-0 p-0 transition-colors",
                truncatePubkey(hex),
                onClick --> (_.foreach(_ => window.navigator.clipboard.writeText(hex)))
              )
            ),
            div(
              cls := "flex items-center gap-2",
              span(cls := "text-xs text-slate-500", "Npub:"),
              button(
                cls := "font-mono text-sm text-slate-200 hover:text-primary-400 cursor-pointer bg-transparent border-0 p-0 transition-colors",
                truncatePubkey(NIP19.encode(XOnlyPublicKey(ByteVector32.fromValidHex(hex)))),
                onClick --> (_.foreach(_ =>
                  window.navigator.clipboard.writeText(
                    NIP19.encode(XOnlyPublicKey(ByteVector32.fromValidHex(hex)))
                  )
                ))
              )
            )
          )
        )
      ),
      if switchButtonLabel.nonEmpty && nip07Available then
        Some(div(
          cls := "flex-shrink-0",
          select.withSelf { self =>
            (
              cls := "text-sm bg-slate-800 hover:bg-slate-700 border border-slate-700 text-slate-300 font-medium px-3 py-1.5 rounded-md cursor-pointer transition-all",
              onChange --> (_.foreach(_ =>
                self.value.get.flatMap {
                  case "nip07" => onSwitchToNip07
                  case "debug" => onSwitch
                  case _ => IO.unit
                }
              )),
              if signerType == "NIP-07" then
                List(
                  option(value := "nip07", selected := true, "NIP-07"),
                  option(value := "debug", "Debugging")
                )
              else
                List(
                  option(value := "debug", selected := true, "Debugging"),
                  option(value := "nip07", "NIP-07")
                )
            )
          }
        ))
      else if switchButtonLabel.nonEmpty then
        Some(div(
          cls := "flex-shrink-0",
          button(
            Styles.buttonSmall,
            switchButtonLabel,
            onClick --> (_.foreach(_ => onSwitch))
          )
        ))
      else None
    )

  def actionCard(
      title: String,
      description: String,
      buttons: Resource[IO, HtmlElement[IO]]*
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      Styles.card,
      div(Styles.cardTitle, title),
      div(Styles.buttonGroup, buttons.toList),
      if description.nonEmpty then
        Some(div(Styles.actionDescription, description))
      else None
    )

  def render32Bytes(
      store: Store,
      bytes32: ByteVector32,
      window: Window[IO]
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      cls := "flex flex-col gap-4",
      entry("Canonical Hex", bytes32.toHex),
      cardWithIcon(
        "If This is a public key",
        "./assets/unlock.svg",
        div(
          cls := "flex flex-col gap-2",
          nip19_21(
            store,
            "npub",
            NIP19.encode(XOnlyPublicKey(bytes32)),
            window
          ),
          nip19_21(
            store,
            "nprofile",
            NIP19.encode(ProfilePointer(XOnlyPublicKey(bytes32))),
            window
          )
        )
      ),
      cardWithIcon(
        "If This is a private key",
        "./assets/lock.svg",
        div(
          cls := "flex flex-col gap-2",
          (store.nip07signer: Signal[IO,Resource[IO,NIP07Signer[IO]]]).map{ signer =>
            signer.use(_.publicKey).map(_ == PrivateKey(bytes32).publicKey.xonly)
            .toResource.flatMap{ alreadyUsingThisPubkey =>
                fixableEntry(
                  "nsec",
                  NIP19.encode(PrivateKey(bytes32)),
                  fixWith = (
                    IO(PrivateKey(bytes32))
                    .map(NIP07.mkDebuggingSigner).flatMap(store.nip07signer.set)
                  ),
                  buttonLabel = "Use as Debugging Key",
                  selectLink = Some(
                    selectable(
                      store,
                      NIP19.encode(PrivateKey(bytes32))
                    )
                  ),
                  enable = !alreadyUsingThisPubkey
                )
            }
          },
          nip19_21(
            store,
            "npub",
            NIP19.encode(PrivateKey(bytes32).publicKey.xonly),
            window
          ),
          nip19_21(
            store,
            "nprofile",
            NIP19.encode(ProfilePointer(PrivateKey(bytes32).publicKey.xonly)),
            window
          )
        )
      ),
      cardWithIcon(
        "If this is an event ID",
        "./assets/braces.svg",
        div(
          cls := "flex flex-col gap-2",
          nip19_21(
            store,
            "nevent",
            NIP19.encode(EventPointer(bytes32.toHex)),
            window
          )
        )
      )
    )

  def renderEventPointer(
      store: Store,
      evp: snow.EventPointer,
      window: Window[IO]
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      cls := "text-md flex flex-col gap-2",
      entry(
        "Event ID (Hex)",
        evp.id,
        Some(selectable(store, evp.id))
      ),
      relayHints(store, evp.relays),
      evp.author.map { pk =>
        entry("Author (Pubkey Hex)", pk.value.toHex)
      },
      evp.kind.map { kind =>
        entry("Kind", kind.toString)
      },
      nip19_21(store, "nevent", NIP19.encode(evp), window),
    )

  def renderProfilePointer(
      store: Store,
      pp: snow.ProfilePointer,
      sk: Option[PrivateKey] = None,
      window: Window[IO]
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      cls := "text-md flex flex-col gap-2",
      sk.map { k =>
        entry(
          "Private Key (Hex)",
          k.value.toHex,
          Some(selectable(store, k.value.toHex))
        )
      },
      sk.map { k =>
        (store.nip07signer: Signal[IO,Resource[IO,NIP07Signer[IO]]]).map{ signer =>
          signer.use(_.publicKey).map(_ == k.publicKey.xonly).toResource.flatMap{
            alreadyUsingThisPubkey =>
              fixableEntry(
                "nsec",
                NIP19.encode(k),
                fixWith = (
                  IO(NIP07.mkDebuggingSigner(k)).flatMap(store.nip07signer.set)
                ),
                buttonLabel = "Use as Debugging Key",
                selectLink = Some(selectable(store, NIP19.encode(k))),
                enable = !alreadyUsingThisPubkey
              )
          }
        }
      },
      entry(
        "Public Key (Hex)",
        pp.pubkey.value.toHex,
        Some(selectable(store, pp.pubkey.value.toHex))
      ),
      relayHints(
        store,
        pp.relays,
        dynamic = if sk.isDefined then false else true
      ),
      entry(
        "npub",
        NIP19.encode(pp.pubkey),
        Some(selectable(store, NIP19.encode(pp.pubkey)))
      ),
      nip19_21(store, "nprofile", NIP19.encode(pp), window)
    )

  def renderAddressPointer(
      store: Store,
      addr: snow.AddressPointer,
      window: Window[IO]
  ): Resource[IO, HtmlDivElement[IO]] = {
    val nip33atag =
      s"${addr.kind}:${addr.author.value.toHex}:${addr.d}"

    div(
      cls := "text-md flex flex-col gap-2",
      entry("Author (Pubkey Hex)", addr.author.value.toHex),
      entry("Identifier (d tag)", addr.d),
      entry("Kind", addr.kind.toString),
      relayHints(store, addr.relays),
      nip19_21(store, "naddr", NIP19.encode(addr), window),
      entry("NIP-33 'a' Tag", nip33atag, Some(selectable(store, nip33atag)))
    )
  }

  def renderEvent(
      store: Store,
      event: Event,
      window: Window[IO]
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      cls := "text-md flex flex-col gap-2",
      if event.isValid then
        Some(renderSubmitEvent(store, event))
      else None,
      if event.pubkey.isEmpty then
        Some(
          div(
            cls := "flex items-center",
            entry("Missing", "Pubkey"),
            button(
              Styles.buttonSmall,
              "Fill with a Debugging Key",
              onClick --> (_.foreach { _ =>
                store.input.set(
                  event
                    .copy(pubkey = Some(keyOne.publicKey.xonly))
                    .asJson
                    .printWith(jsonPrinter)
                )
              })
            )
          )
        )
      else None,
      if event.id.isEmpty then
        Some(
          div(
            cls := "flex items-center",
            entry("Missing", "ID"),
            if event.pubkey.isDefined then
              Some(
                button(
                  Styles.buttonSmall,
                  "Fill ID",
                  onClick --> (_.foreach(_ =>
                    store.input.set(
                      event
                        .copy(id = Some(event.hash.toHex))
                        .asJson
                        .printWith(jsonPrinter)
                    )
                  ))
                )
              )
            else None
          )
        )
      else None,
      if event.sig.isEmpty then
        Some(
          div(
            cls := "flex items-center",
            entry("Missing", "Signature"),
            if event.id.isDefined && event.pubkey == Some(
                keyOne.publicKey.xonly
              )
            then
              Some(
                button(
                  Styles.buttonSmall,
                  "Sign",
                  onClick --> (_.foreach(_ =>
                    store.nip07signer.get.flatMap(_.use { signer =>
                      for
                        pubkey <- signer.publicKey
                        preparedEvent <- IO(event.copy(pubkey = Some(pubkey), id = None))
                        _ <- signer.signEvent(preparedEvent).flatMap{ signedEvent =>
                          store.input.set(
                          signedEvent
                          .asJson
                          .printWith(jsonPrinter)
                        )
                      }
                      yield ()
                    })
                  ))
                )
              )
            else None
          )
        )
      else None,
      entry("Serialized Event", event.serialized),
      entry("Implied Event ID", event.hash.toHex),
      event.id == Some(event.hash.toHex) match {
        case true => entry(
          "Implied Event ID Matches Given Event ID?",
          "Yes"
        )
        case false => fixableEntry(
          "Implied Event ID Matches Given Event ID?",
          "No",
          fixWith = store.input.set(
            event
              .copy(id = Some(event.hash.toHex))
              .asJson
              .printWith(jsonPrinter)
          )
        )
      },
      event.isValid match {
        case true => entry(
          "Is Signature Valid?",
          "Yes"
        )
        case false => fixableEntry(
          "Is Signature Valid?",
          "No",
          buttonLabel = "Sign and Fix",
          notice = "Note: Fixing will update pubkey, ID, and signature",
          fixWith = store.nip07signer.get.flatMap(_.use { signer =>
            for
              pubkey <- signer.publicKey
              preparedEvent <- IO(event.copy(pubkey = Some(pubkey), id = None))
              _ <- signer.signEvent(preparedEvent).flatMap{ signedEvent =>
                store.input.set(
                signedEvent
                .asJson
                .printWith(jsonPrinter)
              )
            }
            yield ()
          })
        )
      },
      // ensure timetsamp is reasonable (before Jan 1, 3000), offer to fix if Not
      (event.created_at >= 0L && event.created_at <= 32_503_680_000L) match
        case true => None // No need to show anything
        case false => Some(
          fixableEntry(
            "Is Timestamp Valid?",
            "No",
            buttonLabel = "Fix with Current Time",
            notice = "Note: Fixing will update ID",
            fixWith = store.input.set(
              event
                .copy(created_at = new java.util.Date().getTime() / 1000)
                .asJson
                .printWith(jsonPrinter)
            )
          )
        ),
      if event.kind >= 30000 && event.kind < 40000 then
        event.pubkey
          .map(author =>
            nip19_21(
              store,
              "naddr",
              NIP19.encode(
                AddressPointer(
                  d = event.tags
                    .collectFirst { case "d" :: v :: _ => v }
                    .getOrElse(""),
                  kind = event.kind,
                  author = author,
                  relays = List.empty
                )
              ),
              window
            )
          )
      else
        event.id.map(id =>
          nip19_21(
            store,
            "nevent",
            NIP19.encode(EventPointer(id, author = event.pubkey, kind = Some(event.kind))),
            window
          )
        )
    )

  def renderSubmitEvent(
    store: Store,
    event: Event
  ): Resource[IO, HtmlDivElement[IO]] =
    if(event.isValid) then
      SignallingRef[IO].of(Option.empty[Messages.FromRelay.OK]).toResource.flatMap {
        relayReply =>
          div(
            cls := "flex items-center space-x-3",
            span(cls := "font-bold text-accent-500", "Submit to Relay? "),
            div(
              cls := "flex flex-wrap justify-between max-w-xl gap-2",
              renderSubmitToRelay(store,event,"ws://localhost:10547"),
              renderSubmitToRelay(store,event,"wss://relay.damus.io"),
              renderInputCustomRelay(store,event)
            )
          )
      }
    else
      div("Invalid event; canNot yet submit to relay")

  def renderSubmitToRelay(
    store: Store,
    validEvent: Event,
    initialRelayUri: String,
    submitOnFirstLoad: Boolean = false
  ): Resource[IO, HtmlDivElement[IO]] =
    (
      SignallingRef[IO].of(false).toResource,
      SignallingRef[IO].of(Option.empty[Messages.FromRelay.OK]).toResource
    ).tupled.flatMap {
      (awaitingReply, relayReply) =>
        val submitEvent =
          IO.fromEither(org.http4s.Uri.fromString(initialRelayUri))
            .toResource
            .flatMap(Relay.mkResourceForIO(_))
            .use(relay =>
              awaitingReply.set(true)
              *> (relay.submitEvent(validEvent).option >>= relayReply.set)
              <* (awaitingReply.set(false))
            ).recoverWith{
              case e: java.io.IOException =>
                relayReply.set(Some(Messages.FromRelay.OK("",false,"Websocket connection error")))
                *> awaitingReply.set(false)
            }

        val buttonLabel = (awaitingReply: Signal[IO,Boolean]).flatMap {
          case false => relayReply.map {
            case None => s"$initialRelayUri"
            case Some(Messages.FromRelay.OK(_,accepted,_))
              if accepted => s"$initialRelayUri - \u2705"
            case Some(Messages.FromRelay.OK(_,accepted,message))
              if !accepted => s"$initialRelayUri - Failed - $message"
            case _ => s"$initialRelayUri"
          }
          case true => relayReply.map(_ => s"$initialRelayUri - ...")
        }

        val isConnectionError = relayReply.map{
          case Some(Messages.FromRelay.OK(_,accepted, msg))
            if(!accepted && msg.contains("connection error")) => true
          case _ => false
        }

        val onFirstLoad = if(submitOnFirstLoad) then submitEvent else IO.unit

        onFirstLoad.background *>
        div(
          isConnectionError.map{
            case false =>
              div(
                cls := "flex items-center rounded",
                button(
                  Styles.buttonSmall,
                  buttonLabel,
                  onClick --> (_.foreach{_ => submitEvent }),
                  disabled <-- awaitingReply
                )
              )
            case true =>
              renderInputCustomRelay(store,validEvent, initialButtonLabel = "Websocket Connection Error")
          }
        )
    }

  def renderInputCustomRelay(
    store: Store,
    validEvent: Event,
    initialButtonLabel: String = "Custom Relay"
  ): Resource[IO, HtmlDivElement[IO]] = (
    SignallingRef[IO].of(false).toResource,
    SignallingRef[IO].of("").toResource
  ).flatMapN{
    (isActive, rawRelayUri) =>
      div(
        (isActive: Signal[IO,Boolean]).map {
          case true =>
            div(
              rawRelayUri.map {
                case url if url.isEmpty =>
                    input.withSelf { self =>
                      (
                        cls := "w-full py-1.5 px-3 text-sm font-moNo rounded-lg bg-slate-800 border border-slate-700 text-slate-100 placeholder:text-slate-500 focus:outline-None focus:ring-2 focus:ring-primary-500/50 focus:border-primary-500 transition-all",
                        defaultValue := "ws://localhost:10547",
                        onKeyPress --> (_.foreach(evt =>
                          evt.key match {
                            case "Enter" =>
                              self.value.get
                              .map{
                                case url if url.isEmpty => url
                                case url if url.contains("://") => url
                                case url if !url.contains("://") && url.nonEmpty => url.prependedAll("wss://")
                                case _ => ""
                              }.flatMap(url =>
                                if url.contains("://") then
                                  rawRelayUri.set(url)
                                else IO.unit
                              )
                            case _ => IO.unit
                          }
                        ))
                      )
                    }
                case url =>
                    renderSubmitToRelay(store,validEvent,initialRelayUri = url, submitOnFirstLoad = true)
              }
            )
          case false =>
            div(
              cls := "flex items-center rounded",
              button(
                Styles.buttonSmall,
                initialButtonLabel,
                onClick --> (_.foreach{_ => isActive.set(true) }),
              )
            )
        }
      )
  }

  private def fixableEntry(
    key: String,
    value: String,
    fixWith: => IO[Unit],
    selectLink: Option[Resource[IO, HtmlSpanElement[IO]]] = None,
    buttonLabel: String = "fix",
    notice: String = "",
    enable: Boolean = true // whether to actually dispay the fix stuff
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      cls := "flex flex-col gap-2",
      div(
        cls := "flex items-center space-x-3",
        span(cls := "font-bold text-slate-300", key + " "),
        span(cls := "font-moNo max-w-xl break-all text-slate-400", value),
        selectLink
      ),
      if enable then
        Some(div(
          cls := "flex flex-col gap-1 pl-0",
          button(
            buttonLabel,
            Styles.buttonSmall,
            onClick --> (_.foreach{_ => fixWith})
          ),
          if(notice.nonEmpty) then
            Some(span(cls := "font-moNo max-w-xl break-all text-slate-500 text-xs", notice))
          else None
        ))
      else None
    )

  private def entry(
      key: String,
      value: String,
      selectLink: Option[Resource[IO, HtmlSpanElement[IO]]] = None
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      cls := "flex items-center space-x-3",
      span(cls := "font-bold text-slate-300", key + " "),
      span(cls := "font-moNo max-w-xl break-all text-slate-400", value),
      selectLink
    )

  private def nip19_21(
      store: Store,
      key: String,
      code: String,
      window: Window[IO]
  ): Resource[IO, HtmlDivElement[IO]] =
    div(
      span(cls := "font-bold text-slate-300", key + " "),
      span(cls := "font-moNo break-all text-slate-400", code),
      button(
        cls := "inline cursor-pointer bg-transparent border-0 p-0",
        onClick --> (_.foreach(_ =>
          window.navigator.clipboard.writeText(code)
        )),
        copyIcon
      ),
      selectable(store, code),
      a(
        href := "Nostr:" + code,
        external
      )
    )

  private def relayHints(
      store: Store,
      relays: List[String],
      dynamic: Boolean = true
  ): Resource[IO, HtmlDivElement[IO]] =
    if !dynamic && relays.isEmpty then div("")
    else
      SignallingRef[IO].of(false).toResource.flatMap { active =>
        div(
          cls := "flex items-center space-x-3",
          span(cls := "font-bold text-slate-300", "Relay Hints "),
          if relays.size == 0 then div("")
          else
            // displaying each relay hint
            div(
              cls := "flex flex-wrap max-w-xl",
              relays
                .map(url =>
                  div(
                    cls := "font-moNo text-xs flex items-center rounded-lg py-1 px-2 mr-1 mb-1 bg-primary-900/40 border border-primary-800/50 text-primary-300",
                    url,
                    // removing a relay hint by clicking on the x
                    div(
                      cls := "cursor-pointer ml-2 text-red-400 hover:text-red-300 transition-colors",
                      onClick --> (_.foreach(_ => {
                        store.result.get.flatMap(result =>
                          store.input.set(
                            result
                              .map {
                                case a: AddressPointer =>
                                  NIP19
                                    .encode(
                                      a.copy(relays =
                                        relays.filterNot(_ == url)
                                      )
                                    )
                                case p: ProfilePointer =>
                                  NIP19
                                    .encode(
                                      p.copy(relays =
                                        relays.filterNot(_ == url)
                                      )
                                    )
                                case e: EventPointer =>
                                  NIP19
                                    .encode(
                                      e.copy(relays =
                                        relays.filterNot(_ == url)
                                      )
                                    )
                                case r => ""
                              }
                              .getOrElse("")
                          )
                        )
                      })),
                      "×"
                    )
                  )
                )
            )
          ,
          active.map {
            case true =>
              div(
                input.withSelf { self =>
                  (
                    cls := "w-full py-1.5 px-3 text-sm font-moNo rounded-lg bg-slate-800 border border-slate-700 text-slate-100 placeholder:text-slate-500 focus:outline-None focus:ring-2 focus:ring-primary-500/50 focus:border-primary-500 transition-all",
                    onKeyPress --> (_.foreach(evt =>
                      // confirm adding a relay hint
                      evt.key match {
                        case "Enter" =>
                          self.value.get
                          .map{
                            case url if url.isEmpty => url
                            case url if url.contains("://") => url
                            case url if !url.contains("://") && url.nonEmpty => url.prependedAll("wss://")
                            case _ => ""
                          }
                          .flatMap(url =>
                            if url.contains("://")
                            then {
                              store.result.get.flatMap(result =>
                                store.input.set(
                                  result
                                    .map {
                                      case a: AddressPointer =>
                                        NIP19
                                          .encode(
                                            a.copy(relays = a.relays :+ url)
                                          )
                                      case p: ProfilePointer =>
                                        NIP19
                                          .encode(
                                            p.copy(relays = p.relays :+ url)
                                          )
                                      case e: EventPointer =>
                                        NIP19
                                          .encode(
                                            e.copy(relays = e.relays :+ url)
                                          )
                                      case r => ""
                                    }
                                    .getOrElse("")
                                )
                              )
                                >> active.set(false)
                            } else IO.unit
                          )
                        case _ => IO.unit
                      }
                    ))
                  )
                }
              )
            case false if dynamic =>
              // button to add a new relay hint
              button(
                Styles.buttonSmall,
                "Add Relay Hint",
                onClick --> (_.foreach(_ => active.set(true)))
              )
            case false => div("")
          }
        )
      }

  private def selectable(
      store: Store,
      code: String
  ): Resource[IO, HtmlSpanElement[IO]] =
    span(
      store.input.map(current =>
        if current == code then a("")
        else
          a(
            href := "#/" + code,
            onClick --> (_.foreach(evt =>
              evt.preventDefault >>
                store.input.set(code)
            )),
            edit
          )
      )
    )

  private val copyIcon = img(cls := "inline w-4 ml-2", src := "./assets/copy.svg")
  private val edit = img(cls := "inline w-4 ml-2", src := "./assets/edit.svg")
  private val external = img(cls := "inline w-4 ml-2", src := "./assets/ext.svg")
}
