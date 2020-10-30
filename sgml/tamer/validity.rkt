#lang typed/racket/base

(require "validity.txml")

(xml-document*-valid? validity*.xml)
