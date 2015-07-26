var fs = require('fs');
var path = require('path');

function tupleIze(line, tupleLength) {
  toks = line.split(' ');
  var res = [];
  for (var i = 0; i < toks.length-tupleLength+1; ++i) {
    var tuple = [];
    var sep = '';
    if (!toks[i]) {
      continue;
    }
    for (var j = i; tuple.length < tupleLength && j < toks.length; ++j) {
      var tok = toks[j];
      if (!tok) {
        continue;
      }
      // period means end of tuple
      if(tok === '.') {
        break;
      }
      tuple.push(tok);
    }
    if (tuple.length === tupleLength) {
      res.push(tuple.join(' '));
    }
  }
  return res.join('\n');
}

function main(dir, argv) {
  var fn = argv[2];
  var outdir = argv[3];
  var tupleLength = Number(argv[4]);
  if (argv.length !== 5 || !fn || !outdir) {
    console.log('invalid: <infile> <outdir> <tupleLength>',fn,tupleLength); // DONOTCHECKIN
    return;
  }
  if (!fs.existsSync(outdir)) {
    console.error('outdir does not exist',outdir);
    return;
  }
  var p = path.parse(fn);
  var outFn = path.join(outdir, p.name+'_'+tupleLength+'Tuples'+p.ext);  
  console.log('reading from',fn,'making tuples',tupleLength,'writing to',outFn);
  lines = fs.readFileSync(fn, {encoding: 'utf8'}).split('\n');
  console.log('read file\tline count',lines.length); // DONOTCHECKIN
  if (fs.existsSync(outFn)) {
    console.log('removing existing',outFn); // DONOTCHECKIN
    fs.unlinkSync(outFn);
  }
  var tuples = [];
  for (var i = 0; i < lines.length; ++i) {
    var line = lines[i];
    var tuplesFromLine = tupleIze(line, tupleLength);
    if (!tuplesFromLine.length) {
      continue;
    }
    tuples.push(tuplesFromLine);
    if (tuples.length > 50000) {
      writeTuples(i);
    }
  }
  console.log('processed\tline count',tuples.length); // DONOTCHECKIN
  console.log('tuples0',tuples[0]);
  writeTuples(i);
  function writeTuples(lineIdx) {
    console.log('write tuples',lineIdx,'of',lines.length,' ',lineIdx/lines.length+'%'); // DONOTCHECKIN
    fs.appendFileSync(outFn,tuples.join('\n'), {encoding:'utf8'});
    tuples = [];
  }
}

main(process.cwd(), process.argv);
