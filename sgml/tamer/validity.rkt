#lang typed/racket/base

(require "validity.xml")

(xml-document*-valid? validity*.xml)
