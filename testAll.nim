import std/[os, osproc, strutils, strformat]

const
  green = "\e[32m"
  red   = "\e[31m"
  cyan  = "\e[36m"
  reset = "\e[0m"

let testsDir = "tests"

if not dirExists(testsDir):
  echo "No tests/ directory found"
  quit(1)

var passed: seq[string]
var failed: seq[string]

for kind, groupPath in walkDir(testsDir):
  if kind != pcDir:
    continue

  echo &"{cyan}[Group] {lastPathPart(groupPath)}{reset}"

  for fkind, filePath in walkDir(groupPath):
    if fkind != pcFile or not filePath.endsWith(".nim"):
      continue

    let (output, code) = execCmdEx(&"nim c -r --hints:off --warnings:off {filePath}")

    let binPath = filePath.changeFileExt(ExeExt)
    if fileExists(binPath):
      removeFile(binPath)

    if code == 0:
      echo &"  {green}[OK]{reset}   {filePath}"
      passed.add(filePath)
    else:
      echo &"  {red}[FAIL]{reset} {filePath}"
      echo output
      failed.add(filePath)

echo ""
echo &"{green}Passed: {passed.len}{reset}"
for p in passed:
  echo &"  {green}+{reset} {p}"
echo &"{red}Failed: {failed.len}{reset}"
for f in failed:
  echo &"  {red}-{reset} {f}"

if failed.len > 0:
  quit(1)