#!/bin/bash
set -o errexit -o nounset
PKG_REPO=$PWD
cd ..

addToDrat(){
  mkdir drat
  cd drat

  ## Set up Repo parameters
  git init
  git config user.name "DeclareDesign Travis"
  git config user.email "team@declaredesign.org"
  git config --global push.default simple

  ## Get drat repo
  git remote add upstream "https://$GH_TOKEN@github.com/DeclareDesign/declaredesign.github.io.git"
  git fetch upstream 2>err.txt
  git checkout master

  Rscript -e "for(pkg in dir('..', pattern = ifelse(.Platform$OS.type == 'windows', '.zip', '.t*z'))) { \
  drat::insertPackage(paste0(ifelse(.Platform$OS.type == 'windows', '..\\', '../', pkg)), \
  repodir = '.', \
  commit='Travis update ${APPVEYOR_PROJECT_NAME:-$PKG_REPO} build ${APPVEYOR_REPO_COMMIT:-$TRAVIS_COMMIT}', branch = 'master') }"

  git push 2>err.txt

}

addToDrat
