#!/usr/bin/env bash

set -exo pipefail

operation="$1"; shift

function sbtSequential() {
  bash -c "cd scala-2 && sbt $1"
  bash -c "cd scala-3 && sbt $1"
}

function sbtParallel() {
  bash -c "cd scala-2 && sbt $1" &
  bash -c "cd scala-3 && sbt $1" &
  wait
}

function clean() {
  sbtParallel clean
}

function compile() {
  sbtParallel compile
}

function test() {
  sbtParallel test
}

function generate() {
  sbtParallel generate/run
}

function publish() {
  sbtSequential publish
}

function publishLocal() {
  sbtSequential publishLocal
}

function gitRelease() {
  sbtSequential gitRelease
}

$operation
