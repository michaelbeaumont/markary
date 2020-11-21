#!/usr/bin/env runhaskell

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Text.Pandoc.JSON

main = toJSONFilter selfLinkHeaders

selfLinkHeaders :: Block -> Block
selfLinkHeaders = \case
  h@(Header level attr (Link {} : _)) -> h
  Header level attr@(id, _, _) is -> Header level attr [Link nullAttr is ("#" <> id, "")]
  b -> b
