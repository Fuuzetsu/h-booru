-- |
-- Module      :  HBooru.Network
-- Copyright   :  (c) Mateusz Kowalczyk 2013
-- License     :  GPL-3
--
-- Maintainer  :  fuuzetsu@fuuzetsu.co.uk
-- Stability   :  experimental
--
-- Module providing functions to interface with some booru sites.
-- Amongst other things, it should (semi-transparently) handle post count
-- limits. The user should simply be able to ask for all images with certain
-- rather than worrying about hard limits per page set by the sites &c.
module HBooru.Network where

import HBooru.Types
