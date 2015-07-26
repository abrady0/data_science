var cp = require('child_process');
var fs = require('fs');
var path = require('path');

function main(dir, argv) {
  var indir = argv[2];
  var outdirBase = argv[3];
  var maxTuples = Number(argv[4]);
  if (argv.length !== 5 || !indir || !outdirBase || !maxTuples) {
    console.log('invalid: <indir> <outdirBase>',indir,outdirBase,maxTuples);
    return;
  }
  if (!fs.existsSync(outdirBase)) {
    console.error('outdirBase does not exist, creating',outdirBase);
    fs.mkdirSync(outdirBase);
  }

  var cleanDir = processDir(indir, 'clean', 'clean.js');
  var tuplizeDir;
  for (var i = 0; i < maxTuples; ++i) {
    tuplizeDir = processDir(cleanDir, 'tupelIze', 'tupleIze.js', i+1);
  }
  processDir(tuplizeDir, 'aggregatize', 'aggregatize.js');

  function processDir(indir, outdirName, scriptName, extraArgs) {
    var outDir = path.join(outdirBase,outdirName);
    if (!fs.existsSync(outDir)) {
      console.log('out dir does not exist, creating',outDir);
      fs.mkdirSync(outDir);
    }
    srcFiles = fs.readdirSync(indir);
    for (var i = srcFiles.length - 1; i >= 0; i--) {
      var fn = srcFiles[i];
      if (path.extname(fn) !== '.txt') {
        console.log('ignoring file',fn); // DONOTCHECKIN
        srcFiles.splice(i,1);
      }
    }
    srcFiles.map(function(fn) {
      var execCmd = 'node '+path.join(__dirname,scriptName)+' '+path.join(indir,fn)+' '+outDir + ' '+(extraArgs || '');
      console.log('exec',execCmd); // DONOTCHECKIN
      cp.execSync(execCmd);
    });
    return outDir;
  }
}

main(process.cwd(), process.argv);
