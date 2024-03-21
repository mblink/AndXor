#!/usr/bin/env bash

set -exo pipefail

operation="$1"; shift
sequential="$([ "$1" = 'sequential' ] && echo 1 || echo 0)"

function sbtSequential() {
  bash -c "cd scala-2 && sbt $1"
  bash -c "cd scala-3 && sbt $1"
}

function sbtParallel() {
  if [ "$sequential" = 1 ]; then
    sbtSequential $1
    return
  fi

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

function testCompile() {
  sbtParallel Test/compile
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

function mimaReport() {
  sbtSequential mimaReportBinaryIssues
}

function docs() {
  sbtParallel docs/mdoc
}

$operation
