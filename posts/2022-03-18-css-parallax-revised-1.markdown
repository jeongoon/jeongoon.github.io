---
title: Css Parallax Effect revised with css grid.
description: show example css parallax applied.
keywords: css, parallax, grid
author: Myoungjin Jeon
tags: css, parallax, grid
---

# Css Parallax Series

1. [Css Parallax Effect revised in 2022](/posts/2022-03-12-css-parallax-revised-0.html)
2. *Css Parallax Effect revised with css grid*

----

After some investigate about css, I found `grid` is great tool for layout.

# Grid layout

My recommened articles regarding to `grid layout` are:

- [A Complete Guide to Grid from Css-Tricks](https://css-tricks.com/snippets/css/complete-guide-grid/)
- [Common Responsive Layouts with CSS Grid (and some without!)](https://medium.com/samsung-internet-dev/common-responsive-layouts-with-css-grid-and-some-without-245a862f48df)

Only thing which bothers me, grid is relatively recent method.

![Not Supported for iOS until 2016](/images/CssParallax-18Mar2022.caniuse-grid-ios.jpg)

However It is safe to say that most of social media websites stop support older
browsers.(Youtube, Facebook, etc). So let's move on.

The basic layout is just a column with overlapped at the same position.

```ascii
+-------------------------------+
|                               |
|                               |
|           Background          |       -> Background and Cover Content
|            /Cover             |                         (Normally for title)
|                               |          is required to overlapped
+-------------------------------+
| caption:                      |
| any contents after ...        |
.                               .
.                               .

```

# Previous (traditional) method for overlapping

if one element is has `relative` `position` and another has `absolute` `position`
they are overlapped because their position calculated again from the beginning.

a possible pseudo code look like:
```html
...
<div id="container">
  <div id="background">
  </div>
  <div id="cover">
  </div>
</div>
...
```

and css ...

```css
..
#background {
    position: absolute;
    width: 100vw;
    height: 100vh;
}

#cover {
    position: relative;
    width: 100vw;
    height: 100vh;
    z-index: 1;
}
...
```

**and how about if we change it to grid layout?**

## Firstly, make a layout:
```css
#container {
    display: grid;
    grid-template-rows:
        [row-top background-row-top cover-row-top] 100vh
        [cover-row-bottom background-row-bottom] auto
        [row-bottom]
}
```

## And apply the grid row and column at the background as:
```css
#background {
    grid-column: 1 / -1; /* use all columns */
    grid-row: background-row-top / background-row-bottom;
}
```

## Also at cover as:
```css
#cover {
    grid-column: 1 / -1; /* use all columns */
    grid-row: cover-row-top / cover-row-bottom;
    z-index: 1;
}
```

**Note:** Even different variable names are used in here, the original layout
doesn't really ditinguish between `background-row-top` and `cover-row-top`
or `background-row-bottom` and `cover-row-bottom`.

*all the variable in the same bracket indicates the same position.*

But It has advantage if we want use a different layout by just changing
`#container` grid settings:

for example, If you want to place the cover slightly above the center of
the background, this is one possible way to do it:

```css
#container {
    display: grid;
    grid-template-rows:
        [row-top background-row-top cover-row-top] 75vh
        [cover-row-bottom] 25vh
        [background-bottom] auto
        [row-bottom]
}
```

But we don't need to change the `#background` or `#cover`.

# My Second css parallax example

Try [Second Example](/examples/18Mar2022.CssParallax/cssParallaxExample.html) and inspect or view the source on your favourite web browser:

It just look almost same as [First Example](/examples/07Mar2022.CssParallax/cssParallaxExample.html).

However one more thing I'd like to mention is that
`white` background contents after parllaxed background actually consists of
white and blank `div` which posses whole `grid rows`, and the real contents on
the top which only consume each `grid row`.

```html
      ...

      <div class="credits">Credits: NASA/Chris Gunn
      </div>
      <!-- background-skirt can be used as rest background area -->
      <div class="background-skirt background-white">

      <!-- and following contents will be shown on top of above div -->
      <div class="padding-1rem">
        <p>
          For original full explanation of how the Css parallax scroll effects works,
          <a href="https://orangeable.com/tools/css-parallax-scroll">visit this site</a>

      ...
```

so let's see the grid layout first:

```css
.css-parallax-toplevel {
    /* .. snip .. */
    display: grid;
    grid-template-rows:
        [a-group-row-top] auto
        [a-group-row-bottom caption-row-top] auto
        [caption-row-bottom skirt-row-top] auto
        [skirt-row-bottom] auto
        [credit-row-top] auto
        [credit-row-bottom]
}
```

and `background-skirt` defined as:

```css
.css-parallax-toplevel > .background-skirt {
    grid-column: 1 / -1;
    grid-row: skirt-row-top / skirt-row-bottom;
}
```

## What is the benefit?

**You can make a individual element which doesn't rely on location**

*Even tough this is not a only way to set background,* you can create
a rest of background as one element.

Your background will disapear at sometime while scrolling,
so you might need to cover the after background. I call it `background skirt` here.

In this way, once `background-skirt` is created as a individual element,
we don't need to think about the rest of background.

## There are more general benefits when using **grid**

1. **More logical or component-based design**
2. **Better control for responsive web site**

One more thing I'd like to mention is that
we can focus our layout in the `grid template` and apply it later,
no matter where you write down your *children* element in your grid div.

In this way, we can think our contents structure in more logical way.
Or you could get advantage when you are planning the different layouts
when designing responsive web sites.

Im my example, `credits` divsion is placed before the actual articles
but it shows in the end as a footer if you look at the html source code.
compare: [First Example](/examples/07Mar2022.CssParallax/cssParallaxExample.html)
and [Second Example](/examples/18Mar2022.CssParallax/cssParallaxExample.html)

# Conclusion

Modern websites offer more convenient concepts like `grid`.
Why don't you adopt the new feature rather than embrace the unsupported or
out-dated web-browsers in 2022?


Okay, That's all for today!
