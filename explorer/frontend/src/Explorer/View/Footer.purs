module Explorer.View.Footer (footerView) where

import Prelude

import Data.Foldable (for_)
import Data.Lens ((^.))
import Data.String (take)
import Explorer.I18n.Lang (Language, langCode, translate)
import Explorer.I18n.Lenses (footer, fooBccOpenSource, fooBccHub
  , fooBccChat, fooBccForum, fooDisclaimerPt1, fooDisclaimerPt2, fooBccFoundation
  , fooEmail, fooGithub, fooIohkSupportP, fooBccDocumentation, fooBccTestnet
  , fooBccSource, fooBccFoundationYoutube, fooBccFoundationTwitter
  , fooKlarityPlatform, fooWhyBcc, fooBccRoadmap, fooBccReddit, fooBccCommunity
  , fooTBCO, fooTBCOBlog, fooTBCOYoutube, fooTwitter, fooProject, fooFoundation
  , fooLearnMore, fooProtocol) as I18nL
import Explorer.Lenses.State (lang, testnet)
import Explorer.Types.Actions (Action(..))
import Explorer.Types.State (State)
import Explorer.Util.Config (commitHash, version)
import Explorer.View.Common (langItems)
import Pux.DOM.Events (onClick) as P
import Pux.DOM.HTML (HTML) as P
import Text.Smolder.HTML (div, nav, a, li, p, span, ul) as S
import Text.Smolder.HTML.Attributes (className, href, title) as S
import Text.Smolder.Markup ((!), (#!))
import Text.Smolder.Markup (text) as S

footerView :: State -> P.HTML Action
footerView state =
    let lang' = state ^. lang
        footerHiddenClazz = if (state ^. testnet) then " hide" else ""
    in
    S.div ! S.className ("explorer-footer" <> footerHiddenClazz) $ do
        S.div ! S.className "explorer-footer__top" $ do
            S.div ! S.className "explorer-footer__container" $ do
                S.div ! S.className "explorer-footer__top--content" $ do
                    S.div ! S.className "content__container content__container--left" $ do
                        langView
                        S.a ! S.className "nav__item--link link-open-source"
                            ! S.href "https://opensource.org/licenses/MIT"
                            $ S.text (translate (I18nL.footer <<< I18nL.fooBccOpenSource) lang')
                        S.p ! S.className "disclaimer"
                            $ S.text (translate (I18nL.footer <<< I18nL.fooDisclaimerPt1) lang')
                        S.p ! S.className "disclaimer"
                            $ S.text (translate (I18nL.footer <<< I18nL.fooDisclaimerPt2) lang')
                        navBottomListView $ navBottmItems lang'
                    S.div ! S.className "content__container content__container--right" $ do
                          socialListView $ socialItems lang'
                          S.nav ! S.className "nav__container" $ do
                              navListView $ navItemsLeft lang'
                              navListView $ navItemsRight lang'
        S.div ! S.className "explorer-footer__bottom" $ do
            S.div ! S.className "explorer-footer__container" $ do
                S.div ! S.className "logo__wrapper" $ do
                    S.div ! S.className "logo__container" $ do
                        S.a ! S.className "logo__bcc-name bg-logo-bcc-name"
                            ! S.href "//bcccoin.io/projects/bcc/"
                            $ S.text ""
                    S.span  ! S.className "split"
                            $ S.text ""
                    S.a ! S.className "support"
                        ! S.href "//bcccoin.io/projects/bcc/"
                        $ S.text (translate (I18nL.footer <<< I18nL.fooIohkSupportP) lang')
                    S.div ! S.className "logo__container"
                          $ S.a ! S.className "logo__tbco-name bg-bcccoin-logo"
                                ! S.href "//bcccoin.io/"
                                $ S.text ""
            S.div ! S.className "explorer-footer__container explorer-footer__meta" $ do
                S.span  ! S.className "version"
                        $ S.text ("v. " <> version)
                S.a ! S.className "commit"
                    ! S.href ("https://github.com/The-Blockchain-Company/bcc-sl/commit/" <> commitHash)
                    $ S.text $ "( " <> (take 7 $ commitHash) <> " )"

-- lang

-- TODO(jk) move lagn views into Common.purs

langView :: P.HTML Action
langView =
    S.ul  ! S.className "lang-nav__container"
          $ for_ langItems langItemView

langItemView :: Language -> P.HTML Action
langItemView lang' =
    let flagClazz = "bg-icon-lang-" <> langCode lang' in
    S.li ! S.className ("lang-nav__item " <> flagClazz )
        #! P.onClick (const $ SetLanguage lang')
        $ S.text (show lang')

-- social

type SocialItem =
    { link :: String
    , label :: String
    , iconClazz :: String
    }

socialItems :: Language -> Array SocialItem
socialItems lang =
    [ { label: translate (I18nL.footer <<< I18nL.fooTwitter) lang
      , link: "https://twitter.com/bccstiftung"
      , iconClazz: "bg-icon-twitter"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooEmail) lang
      , link: "mailto:info@bcchub.org"
      , iconClazz: "bg-icon-email"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooGithub) lang
      , link: "https://github.com/The-Blockchain-Company/bcc-sl/"
      , iconClazz: "bg-icon-github"
      }
    ]

socialListView :: Array SocialItem -> P.HTML Action
socialListView items =
    S.ul  ! S.className "social-nav__container"
          $ for_ items socialItemView

socialItemView :: SocialItem -> P.HTML Action
socialItemView item =
    S.li ! S.className "social-nav__item "
         $ S.a  ! S.className "social-nav__item--link"
                ! S.href item.link
                ! S.title item.label
                $ S.span  ! S.className ("icon "
                              <> item.iconClazz)
                          $ S.text ""

-- nav

type NavItem =
    { label :: String
    , link :: String
    }

navItemsLeft :: Language -> Array NavItem
navItemsLeft lang =
    [ { label: translate (I18nL.footer <<< I18nL.fooBccDocumentation) lang
      , link: "//bccdocs.com"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooBccRoadmap) lang
      , link: "https://bccroadmap.com"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooBccTestnet) lang
      , link: "https://testnet.blockchain-company.io"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooBccSource) lang
      , link: "https://github.com/The-Blockchain-Company/bcc-sl"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooBccFoundation) lang
      , link: "https://bccfoundation.org"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooBccHub) lang
      , link: "https://bcchub.org"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooWhyBcc) lang
      , link: "https://whybcc.com"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooBccFoundationYoutube) lang
      , link: "https://www.youtube.com/channel/UCbQ9vGfezru1YRI1zDCtTGg"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooBccFoundationTwitter) lang
      , link: "https://twitter.com/BccStiftung"
      }
    ]

navItemsRight :: Language -> Array NavItem
navItemsRight lang =
    [ { label: translate (I18nL.footer <<< I18nL.fooBccChat) lang
      , link: "https://chat.bcchub.org/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooBccForum) lang
      , link: "https://forum.bcchub.org/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooBccReddit) lang
      , link: "https://www.reddit.com/r/bcc/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooBccCommunity) lang
      , link: "https://bcchub.org"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooKlarityPlatform) lang
      , link: "https://klaritywallet.io"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooTBCO) lang
      , link: "https://bcccoin.io"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooTBCOBlog) lang
      , link: "https://bcccoin.io/blog/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooTBCOYoutube) lang
      , link: "https://www.youtube.com/channel/UCBJ0p9aCW-W82TwNM-z3V2w"
      }
    ]

navListView :: Array NavItem -> P.HTML Action
navListView items =
    S.ul  ! S.className "nav__list"
          $ for_ items navItemView

navItemView :: NavItem -> P.HTML Action
navItemView item =
    S.li ! S.className "nav__item"
         $ S.a  ! S.className "nav__item--link"
                ! S.href item.link
                $ S.text item.label

navBottmItems :: Language -> Array NavItem
navBottmItems lang =
    [ { label: translate (I18nL.footer <<< I18nL.fooProject) lang
      , link: "https://bccfoundation.org/project/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooProtocol) lang
      , link: "https://bccfoundation.org/protocol/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooFoundation) lang
      , link: "https://bccfoundation.org/foundation/"
      }
    , { label: translate (I18nL.footer <<< I18nL.fooLearnMore) lang
      , link: "https://bccfoundation.org/learn-more/"
      }
    ]

navBottomListView :: Array NavItem -> P.HTML Action
navBottomListView items =
    S.ul  ! S.className "nav-bottom__list"
          $ for_ items navBottomItemView

navBottomItemView :: NavItem -> P.HTML Action
navBottomItemView item =
    S.li ! S.className "nav-bottom__item"
         $ S.a  ! S.className "nav-bottom__item--link"
                ! S.href item.link
                $ S.text item.label
