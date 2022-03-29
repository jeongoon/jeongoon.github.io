---
title: Css Parallax for Yesod Widget (WIP)
description: Introduce how to make yesod widget by using `Literate Haskell`
keywords: literate haskell, haskell, lhs, yesod, widget, css, parallax
author: Myoungjin Jeon
---

**This module applys css parallax on Yesod by creating Yesod widget**.

And article written in [Literate Haskell](https://wiki.haskell.org/Literate_programming).

In other words, this article in raw format(*.lhs*) could be actually used in `yesod`.

If you are not familar with yesod, you might need to get more information from:
[yesod website](https://www.yesodweb.com/).

    Table of Contents
    - Synopsis (How to use)
      - Precaution
      - Example Code
    - Css Parallax Expalnation
    - About Yesod Widget
      - Benefit
      - Widget type and Snippet Code.

    - Making Widget
      - Data structure for the widget
        (data of which the widget consists)
      - Add css codes(Lucius) for parallax effect
      - Html (Hamlet)

Synopsis (How to use)
=====================

Precaution
----------

- This module only get tested under [yesod-scaffold](https://www.yesodweb.com/book/scaffolding-and-the-site-template)

- Please remove any padding in the container for `css-parallax-toplevel-#{classSurfix}`
  which includes `gap` for the grid display type.
  the basic box-sizing and border, padding are like below:
  (if you don't include those, you will see unexpected padding
  around the `div`)

```css
* , *::after, *::before {
    box-sizing:border-box;
    border: 0;
    padding: 0;
}
```

**NOTE:** Above settings is NOT included in the Widget.


Example Code
============

the following code shows how to use the widget.

*Note: there are some pseudo function or css class name*

```haskell
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    parallaxSurfix <- newIdent
    master <- getYesod -- required to get copyright information

    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
        parallaxSettings =
          CssParallaxSettings
          { classSurfix = parallaxSurfix
          , perspective = PixelSize . toRational $ (100 :: Integer)
          , globalBgSpeed = Speed $ 1 :% 5

          , customTopLevelClasses = CssClasses []-- [ "body-grid-column-template" ]
          , mbSkirtClasses = Nothing
          , mbFooterContent =
              Just $ toWidget $(hamletFile "templates/toplevel-footer.hamlet")
          , parallaxGroups =
              [ CssParallaxGroup
                { groupId = "intro"
                , customCssClasses = CssClasses []{-[ "whole-grid-column"
                                                -- ^ use whole area of topLevel
                                                , "body-grid-column-template"
                                                ]-}
                , parallaxClasses = CssClasses []{-[ "whole-grid-column"
                                               -- ^ use whole area of group
                                               ]-}
                , mbBgSpeed = Nothing
                , bgStaticRoute = img_cafeblossom_grey_webp
                                  -- ^ from Settings/StaticFiles.hs
                , titleContainerClasses = CssClasses []-- [ "main-grid-area" ]
                , titleContent = toWidget [hamlet|
                                             <h1>
                                               Better Coffee Now
                                          |]
                , moreContents =
                    [
 #if PRODUCTION
                      toWidget $(hamletFile "templates/home-main.hamlet")
                      -- we can add more .. ??
 #else
                      toWidget $(hamletFileReload "templates/home-main.hamlet")
 #endif
                    ]
                }
              ]
           }

    defaultLayout $ do
      --let (commentFormId, commentTextareaId, commentListId) = commentIds

        -- ^ load extra css
        setTitle "Cafe Blossom"
 #if PRODUCTION
        toWidgetHead $(luciusFile "templates/homepage.lucius")
 #else
        toWidgetHead $(luciusFileReload "templates/homepage.lucius")
 #endif
        parallaxWidget parallaxSettings
```
----

Css Parallax Explanation
========================

* [The original css parallax article](https://keithclark.co.uk/articles/pure-css-parallax-websites) by Keith Clark

* [Revised article on my blog](/posts/2022-03-12-css-parallax-revised.html)

concisely, to get 3d transformation we need:

*three elements with some attributes which are:*
```
    1. base element and a perspective from where we see the object on the page.
       (which is generally not moving during transformation)
    2. an anchor element(normally div) contains the elements which is actually
       tranformed.
    3. transformation origin (default is the center of the anchor element)
```

About Yesod Widget
==================
Please read this [online book](https://www.yesodweb.com/book/widgets)

Benefit
-------

- we could build a set of Html, Javascript, Css in one module
- combine with others easily later
- without thinking of monolithic structure of html

and last but not least,

- parameterize your widget by using template interpolation

Firstly, We need to use some haskell extensions.

\begin{code}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies #-}
\end{code}

`QuasiQuotes` required for [hamlet| ... |] style syntax

\begin{code}
module Widget.CssParallax
   ( CssParallaxSettings (..)
   , CssParallaxGroup (..)
   , CssClassesType (..)
   , ClassSurfix
   , SpeedType (..)
   , ScaleType (..)
   , parallaxWidget
   ) where

import Data.Text (Text)
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Yesod.Core (HandlerSite, MonadWidget)
import Yesod.Core.Types (WidgetFor)
import Yesod.Core.Widget (whamlet, toWidget, CssBuilder(..))
import Yesod.Static (StaticRoute)
import Text.Lucius
import Text.Blaze (ToMarkup, toMarkup)
import Foundation
\end{code}

Foundataion in yesod-scaffold creates data type `App`, `Widget`, Handlers
(including `StaticR` which is used for css `bacgkground-image` here)

 ## Widget type and Snippet Code.

Whether we are creating from quasi quotes or loading from file,
results in Widget data type.

from yesodweb.com ther is an example for lucius (template lang for css)

```haskell
footer :: Widget
footer = do
   toWidget
     [lucius|
         footer {
             font-weight: bold;
             text-align: center;
         }
     ]
   toWidget
     [hamlet|
       <footer>
         <p>That's all folks!
     |]
```

So we can later interpolate with other Widgets like:

```haskell
page :: Widget
page =
  [whamlet|
      <p>This is my page. I hope you enjoyed it.
      ^{footer}
  |]
```

Suprisingly, we don't ned to worry about mixed template lanuages.
Above code use Html(Hamlet) and Css(Lucius) at the same time.

So basically every element could be a `Widget`.

We can load our widget(s) later, which is explained in:
[here](https://www.yesodweb.com/book/widgets#widgets_using_widgets)


Making Widget
-------------

Now Let's have a look in to the previous example as an `Yesod Widget`. 
This widget consists of Html and Css only, so we will see how
to create an widget with Hamlet and Cacius

Data structure for the widget
-----------------------------

I am going to put in the page:

- class name prefix
- background image (as StaticRoute)
- title contents (with no parllax effect)
- main contents after the image
  (normally has the background color)

\begin{code}
type ClassSurfix = Text
data CssParallaxSettings site =
  CssParallaxSettings
  { classSurfix           :: ClassSurfix
  , customTopLevelClasses :: CssClassesType
  , perspective           :: PixelSize
  , globalBgSpeed         :: SpeedType
  , mbSkirtClasses        :: Maybe CssClassesType
  , parallaxGroups        :: [CssParallaxGroup site]
  , absParallaxItems      :: [AbsCssParallaxItem site]
  , mbFooterContent       :: Maybe (WidgetFor site ())
  }

-- | each group can have different background and parallax speed.
data CssParallaxGroup site =
  CssParallaxGroup
  { groupId                     :: ParallaxGroupId
    -- ^ used for unique css class name
  , grpCustomCssClasses         :: CssClassesType
    -- ^ this is custom css classes for styling
  , grpParallaxClasses          :: CssClassesType
    -- ^ about parallax object specific styling
  , mbGrpBgSpeed                :: Maybe SpeedType
  , grpBgStaticRoute            :: StaticRoute
  , grpTitleContainerClasses    :: CssClassesType
  , grpTitleContent             :: WidgetFor site ()
  , grpMoreContents             :: [WidgetFor site()]
  }

-- | Some parallax items can has 'absolute' position and moving at different
--   speed. (normally used for faster-moving elements)
data AbsCssParallaxItem site =
  AbsCssParallaxItem
  { itemId                :: ParallaxItemId
  , customItemCssClasses  :: CssClassesType
  , itemSpeed             :: SpeedType
  , itemContent           :: WidgetFor site ()
  }
\end{code}

Make some type alias for more readable form of declaration
----------------------------------------------------------

\begin{code}
-- | alias for parallax group or item Id
type ParallaxGroupId = Text
type ParallaxItemId  = Text

newtype CssClassesType = CssClasses { getCssClasses :: [Text] }
-- |  join all the classes with " " to use inside tag < .. class="<Here>" >
instance Show CssClassesType where
  show (CssClasses ts) = (show . T.unwords) ts
instance ToCss CssClassesType where
  toCss (CssClasses ccs) = toCss . T.unwords $ ccs
instance ToMarkup CssClassesType where
  toMarkup (CssClasses ccs) = toMarkup . T.unwords $ ccs
\end{code}

However, the below code got complained:
```haskell
type SpeedType = Rational
type Scaletype = Rational

instance (Show a, Real a) => ToCss (Ratio a) where
  toCss x = toCss . show $ x
```

```sh
keyword: warningOrphans

    Orphan instance: instance (Show a, Real a) => ToCss (Ratio a)
    To avoid this
        move the instance declaration to the module of the class or of the type, or
        wrap the type with a newtype and declare the instance on the new type.
    |
XXX | > instance (Show a, Real a) => ToCss (Ratio a) where
    |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ ...
```


So I change to

\begin{code}
newtype SpeedType = Speed Rational
newtype ScaleType = Scale Rational

instance ToCss ScaleType where
  toCss (Scale s) = toCss . (show :: Float -> String) . fromRational $ s
instance ToCss SpeedType where
  toCss (Speed s) = toCss . (show :: Float -> String) . fromRational $ s
\end{code}

To avoid class naming colision, you can add `ClassSurfix` via `newIdent`.

[learn more](https://www.yesodweb.com/book/widgets#widgets_generate_ids)

\begin{code}
-- |  Return the appropriate value for translateZ by given perspective and
-- scale value.
-- Original apporach:
--   set translateZ value -> make scale value
-- New approach:
--    set scale value -> make translateZ value
--
-- Because more scale means less moving speed
--      h  : transformed height
--      h' : original height
--      p  : perspective value
--      z  : translated value on Z-axis
--
-- >         h : h' = p : p + |z|
-- >
-- >                h'     (p+|z|)
-- >        scale = ----  = -------
-- >                 h         p
-- >
-- >       |z| = p * scale - p = p * (scale - 1)

getTranslateZpixel :: PixelSize -> ScaleType -> PixelSize
getTranslateZpixel (PixelSize perspecVal) (Scale scaleVal) =
  PixelSize . toRational . negate $
  (fromRational perspecVal :: Float) * (scaleVal' - 1)
  where
    scaleVal' = fromRational scaleVal :: Float

-- |  Create a main widget with 100% covered background with contents
-- TODO: parameterize horizontal cover size
parallaxWidget :: (MonadWidget m, HandlerSite m ~ App) =>
  CssParallaxSettings App -> m ()
parallaxWidget CssParallaxSettings{..} = do
  let bgScaleFromSpeed :: SpeedType -> ScaleType
      bgScaleFromSpeed (Speed spd)
        | spd == 0 = Scale . toRational $ (999999999999999 :: Integer)
        | otherwise = Scale $ 1 / spd
      globalBgScale = bgScaleFromSpeed globalBgSpeed

      translateZpx spd =
        (getTranslateZpixel perspective) . bgScaleFromSpeed $ spd
\end{code}


About luciusMixin
-----------------

[Online Book](https://www.yesodweb.com/book/shakespearean-templates#shakespearean-templates_lucius_syntax)

Some properties can be parameterized and less mistakes expected when writing css.
For example, using `transform` and `-webkit-transform` with same property value.

\begin{code}
-- v will be interpolated in the follwing `toWidget`
      transformForPallaxMixin sp = [luciusMixin|
      transform: translateZ(#{translateZpx sp}) scale(#{bgScaleFromSpeed sp});
       -webkit-transform: translateZ(#{translateZpx sp}) scale(#{bgScaleFromSpeed sp});
       |]
\end{code}

However above mixin value depends on speed value from each different group.
we need to concatenate all the settings. which is done by easily,
because Mixin is a Monoid.

So let's make a list of pair of ParallaxGroupId => SpeedType

```haskell
      parallaxGroupIdToSpeed =
        map (\CssParallaxGroup{..} -> (groupId, mbBgSpeed)) parallaxGroups
```

But probably map with **global** background speed and is better idea.

\begin{code}
      parallaxGroupWithGlobalSpeed :: [(ParallaxGroupId, SpeedType, StaticRoute)]
      parallaxGroupWithGlobalSpeed =
        map (\CssParallaxGroup{..} ->
                (groupId
                , Maybe.maybe globalBgSpeed id mbBgSpeed
                , bgStaticRoute
                )
            )
            parallaxGroups
\end{code}

One more thing I would like to mention is that
all this settings for parllax speed (which is related to transformation scale)
and background is reuquired to remain inside of `@supports` block.
However `@supports` block is unable to get mixin directly in its block,
we need to make a separate lucius block and will be applied later.

\begin{code}
      -- | set up transformation and background for each group
      applyAllGroupsParallaxSetup =
        foldr (>>) (toWidget $ CssBuilder "") $
        map (\(gid, spd, bgRoute) ->
               toWidget [lucius|
                          ##{gid} > .as-parallax {
                            ^{transformForPallaxMixin spd}
                            width: min(105%, 100vw);
                            height: 100%; /* we need to set the size on grid setting*/
                            box-sizing: border-box;
                            background-image: url( @{StaticR bgRoute} );
                            background-repeat: repeat;
                            background-size: cover;
                            background-position: center;
                          }
                        |]
            ) parallaxGroupWithGlobalSpeed
\end{code}

This is another mixin example for duplicated properties.

\begin{code}
      transformForFixingSafariHWAccelMixin = [luciusMixin|
        transform: translateZ(1e-12px);
       -webkit-transform: translateZ(1e-12px);
      |]
\end{code}


Instead of some comments you can use  meaningful function name.

\begin{code}
      gridUseAllColumnsMixin = [luciusMixin|grid-column: 1 / -1;|]
\end{code}

Grid setup is useful when custom layout applied through `customTopLevelClasses`,
`groupClasses`, etc.

The main elements are `parallax groups` a.k.a `slides`.
And also after last group finished, we need an extra `div` to fill up
empty space (especially we have more contents after last background)
And lastly, if you need any footer for your web page, footer should be
inside of toplevel because toplevel handles overflow here.

However, groups could be more than one, we are unable to specify the
grid row names, so you might need to specify only how many rows it will consume

```haskell

```

Each group contains:

1. a parallax object (background image)
   *could be improve with multiple parallax object*
2. cover (normally used for title)
3. group contents

So, grid template looks like:

*TODO* : parameterize cover height

\begin{code}
      gridTemplateGroupMixin = [luciusMixin|
        grid-template-rows:
          [cp-rtop cp-prlx-rtop cp-bs-rtop] 100vh
          [cp-prlx-rbtm cp-bs-rbtm cp-bs-extra-rtop] auto
          [cp-bs-extra-rbtm cp-rbtm];
        |]
\end{code}

rtop: row top, rbtm: row bottom, prlx: parllax, bs: base(no parallax) relatively.
For basic setup  background and cover share the same area.

You can also parameterize the your mixin

\begin{code}
      gridUseAllRowsNamedMixin :: Text -> Mixin
      gridUseAllRowsNamedMixin rowName = [luciusMixin|
        grid-row: cp-#{rowName}-rtop / cp-#{rowName}-rbtm;
      |]
\end{code}

Typeclass like `PixelSize` is amazing when you use cacius or lucius
PixelSize is a `Num` so you can add any `Num` but when tranlated into
Css, which is appened by "px" string.

\begin{code}
      fixiOSCaveatMixin = [luciusMixin|
        perspective: #{perspective + 1};
      |]

\end{code}

To apply(append) custom css classes, I made one more little mixin

\begin{code}
      appendCssClasses :: CssClassesType -> String
      appendCssClasses ccs
        | null (getCssClasses ccs) = ""
        | otherwise = " " <> (show ccs)
\end{code}

And the following code is for the css part...

Add css codes(Lucius) for parallax effect
-----------------------------------------

\begin{code}
  toWidget [lucius|
.css-parallax-toplevel-#{classSurfix} {
  height: 500px; /* fallback for older browser */
  height: 100%;
  width: 100%;
  overflow-x: hidden;
  overflow-y: auto;
  margin: 0;
  padding: 0;
  display: grid;
}
.css-parallax-group-#{classSurfix} {
    ^{gridUseAllColumnsMixin}

    display: grid;
    ^{gridTemplateGroupMixin}
}
.css-parallax-layer-#{classSurfix} {
    ^{gridUseAllColumnsMixin}
    transform-origin: center bottom;
}
.css-parallax-layer-#{classSurfix}.as-parallax {
    /* set another background division after the original background */
    /* this is normally for simple background setup for content
     * which is not parallaxed */
    ^{gridUseAllRowsNamedMixin "prlx"}
}
.css-parallax-layer-#{classSurfix}.as-base {
    ^{gridUseAllRowsNamedMixin "bs"}
}
.css-parallax-layer-#{classSurfix}.as-base-extra {
    ^{gridUseAllRowsNamedMixin "bs-extra"}
}
.css-parallax-item-#{classSurfix} {
    position: absolute;
}
.css-parallax-toplevel-#{classSurfix} > .css-parallax-background-skirt-#{classSurfix} {
    ^{gridUseAllColumnsMixin}
    grid-row: span 1;
/*    ^{gridUseSkirtRowMixin}*/
}
.css-parallax-toplevel-footer-#{classSurfix} {
    ^{gridUseAllColumnsMixin}
    grid-row: span 1;
}
.css-parallax-toplevel-#{classSurfix} + div {
    /* credit: The Safari and iOS Caveat on https://orangeable.com/css/parallax-scroll */
    /* jeongoon: haven't tested. no iPad or Macbook :-/ */

    /* Safari / iOS issue with `overflow: hidden` is not working
     * in the `css-parallax-toplevel` */
    ^{fixiOSCaveatMixin}
}
@supports ((perspective: 1px) and (not (-webkit-overflow-scrolling: touch))) {
    body {
        transform: translateZ(0px);
        /* ^ Fix paint issues in Edge & Safari H/W accel */
    }

    .css-parallax-toplevel-#{classSurfix} {
        perspective: #{perspective};
       /* v fix wrong calculation visual size(height) of background
        *   on webkit based browsers.
        */
       perspective-origin: center bottom;
    }

    .css-parallax-toplevel-#{classSurfix} > *
    , .css-parallax-layer-#{classSurfix}.as-base
    , .css-parallax-layer-#{classSurfix}.as-base-extra {
        z-index: 1;
        /* prevents the browser flattening the `css-parallax-layer` elements */
        transform-style: preserve-3d;
        /* v ensure Safari H/W acceleration */
        ^{transformForFixingSafariHWAccelMixin}
    }
}
|]
  applyAllGroupsParallaxSetup
\end{code}

Html (Hamlet)
-------------

And finally this is Html part for actual displaying layout.
Please have a look `gridTemplateToplevelMixin` and `gridTemplateGroupMixin`
for the structure.

\begin{code}
  toWidget [whamlet|
<div class="css-parallax-toplevel-#{classSurfix}#{appendCssClasses customTopLevelClasses}">
  $if null parallaxGroups
    <div>
      No parallaxGroups: Please Check you parallaxGroups
  $else
    $forall pg <- parallaxGroups
      <div id="#{groupId pg}" class="css-parallax-group-#{classSurfix}#{appendCssClasses (grpCustomCssClasses pg)}">
        <div class="css-parallax-layer-#{classSurfix} as-parallax#{appendCssClasses (grpParallaxClasses pg)}">

        <div class="css-parallax-layer-#{classSurfix} as-base#{appendCssClasses (grpTitleContainerClasses pg)}">
          ^{grpTitleContent pg}
        <div class="css-parallax-layer-#{classSurfix} as-base-extra">
            $forall moreContent <- (grpMoreContents pg)
              ^{moreContent}
      $forall pi <- absParallaxItems
        <div id="#{itemId pi}" class="css-parallax-item-#{classSurfix}#{appendCssClasses (customItemCssClasses pi)}">
          ^{itemContent pi}
  $maybe skirtClasses <- mbSkirtClasses
    <div class="css-parallax-background-skirt-#{classSurfix}#{appendCssClasses skirtClasses }">
  $maybe footerContent <- mbFooterContent
    <div class="css-parallax-toplevel-footer-#{classSurfix}">
      ^{footerContent}
|]
\end{code}


About My Literate Haskell Usage
-------------------------------

When ghc is compling literate haskell file and if something goes wrong,
  it doesn't suggest what to do next. And if there is one line ghc cannot
  understand it will just spit out the error messages like below.



this is a capture for literate pre-processor typo error messages

```sh
src/Widget/CssParallax.lhs line 502: unlit: Program line next to comment
src/Widget/CssParallax.lhs line 504: unlit: Program line next to comment

/.../..../src/Widget/CssParallax.lhs:1:1: error:
    `unlit' failed in phase `Literate pre-processor'. (Exit code: 1)
```

And it only says my first line of code is incorrect.

```sh

  |
1 |  This module applys css parallax on Yesod by creating Yesod widget
  | ^
```

For the first time I met those error, I totally had no idea what to do next.


TODO
====

add support individual parallax item
