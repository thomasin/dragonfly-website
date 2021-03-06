import smoothscroll from 'smoothscroll-polyfill'

import Columns from './_2column-grid'
import Filtering from './filtering'
import Footer from './footer'
import ImageCaptions from './image-caption'
import LazyLoad from './lazy-load'
import MobileMenu from './mobile-menu'
import Parallaxing from './parallax'
import TopSection from './top-section'

import GameOfLife from './game-of-life/game-of-life'
import StickySidebar from './sticky-sidebar'

smoothscroll.polyfill()

Zepto(($: ZeptoStatic) => {
  Columns()
  Filtering()
  MobileMenu()
  TopSection()
  LazyLoad()
  Parallaxing()
  GameOfLife()
  ImageCaptions()
  Footer()
  StickySidebar('.sticky-container__body', '.sticky-container__element')
})
