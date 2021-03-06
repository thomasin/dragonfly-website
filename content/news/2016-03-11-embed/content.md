---
title: Include Regional Economic Activity charts in your website
---
Chris built a nifty feature into the website for the Regional Economic Activity Report (REAR) that lets you embed
selected maps or charts into your own website. The embed feature has been used by New Zealand Herald in the data insights pages.

<!--more-->

The Herald picked out a chart about [tourism spending in Matamata](http://insights.nzherald.co.nz/article/new-zealand-regional-economy) as a hook for a story about regional economies. This example shows how the embed feature can extend the reach of the REAR report website.

![Screenshot of the embed dialog, which generates the code you need to embed charts.](/news/2016-03-11-embed/embed.png)

To include these charts in your website, navigate to a page you are interested in and click on the share icon at the top right
of each chart. This function will bring up the share dialogue, and you can customise which components from the page you want to embed. By pasting the HTML into a page of your website or blog, that specific content will be displayed.

To make things nice and tidy, we have included hooks to the [iFrameResizer javascript library](http://davidjbradshaw.github.io/iframe-resizer/) in the embed. If you include that library in your website, you will be able to dynamically change the size of the embedded chart so that it always displays well on mobile phones.

As an example of embedding in action, data for net international migration to New Zealand for the year to March 2015 is embedded below. Immigration in the most recent year was much higher than in any other year since 1992.

<style>iframe{width:100%}</style>
<iframe src="https://teal-skua-dev.dragonfly.co.nz/theme/international-migration/a/timeseries/2015/new-zealand/?embed=dynamic%26intersection=hide" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" width="600" height="1424"></iframe>
<script>iFrameResize()</script>

The [Regional Economic Activity Report](http://webrear.mbie.govt.nz/summary/new-zealand) was developed by Dragonfly for the Ministry of Business, Innovation and Employment. [Read more about the project](https://www.dragonfly.co.nz/work/webrear-case-study.html).
