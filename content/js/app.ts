import smoothscroll from 'smoothscroll-polyfill'

import Columns from './_2column-grid'
import Filtering from './filtering'
import LazyLoad from './lazy-load'
import MobileMenu from './mobile-menu'
import Parallaxing from './parallax'
import TopSection from './top-section'

import ScrollImages from './scroll-images'

import GameOfLife from './game-of-life/game-of-life'

smoothscroll.polyfill()

Zepto(($: ZeptoStatic) => {
    Columns()
    Filtering()
    MobileMenu()
    TopSection()
    LazyLoad()
    Parallaxing()
    GameOfLife()
    ScrollImages()
})
