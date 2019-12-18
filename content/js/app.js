import smoothscroll from 'smoothscroll-polyfill'
import Zepto from 'zepto'

import LazyLoad from './lazy-load'
import MobileMenu from './mobile-menu'
import Parallaxing from './parallax'
import Publications from './publications'
import TopSection from './top-section'

import GameOfLife from './game-of-life/game-of-life'

smoothscroll.polyfill()

Zepto(($) => {
    Publications($)
    MobileMenu()
    TopSection($)
    LazyLoad()
    Parallaxing()
    GameOfLife()
})