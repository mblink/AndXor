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



function githubWorkflowGenerate() {
  sbtParallel githubWorkflowGenerate

  ghdir='.github/workflows'

  mv "scala-2/$ghdir/clean.yml" "$ghdir/clean.yml"
  rm "scala-3/$ghdir/clean.yml"

  cat "scala-2/$ghdir/ci.yml" | sed -E 's/^(name:)\s*(Continuous Integration)$/\1 Scala 2 \2/' > "$ghdir/scala-2-ci.yml"
  rm "scala-2/$ghdir/ci.yml"

  cat "scala-3/$ghdir/ci.yml" | sed -E 's/^(name:)\s*(Continuous Integration)$/\1 Scala 3 \2/' > "$ghdir/scala-3-ci.yml"
  rm "scala-3/$ghdir/ci.yml"
}

$operation
