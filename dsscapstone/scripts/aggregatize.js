var fs = require('fs');
var path = require('path');

function main(dir, argv) {
  var fn = argv[2];
  var outdir = argv[3];
  if (argv.length !== 4 || !fn || !outdir) {
    console.log('invalid: <infile> <outdir>',fn,outdir); // DONOTCHECKIN
    return;
  }
  if (!fs.existsSync(outdir)) {
    console.error('outdir does not exist',outdir);
    return;
  }
  var p = path.parse(fn);
  var outFn = path.join(outdir, p.name+'_aggregate'+p.ext);  
  console.log('reading from',fn,'writing to',outFn);
  linesIn = fs.readFileSync(fn, {encoding: 'utf8'}).split('\n');
  console.log('read file\tline count',linesIn.length); // DONOTCHECKIN
  if (fs.existsSync(outFn)) {
    console.log('removing existing',outFn); // DONOTCHECKIN
    fs.unlinkSync(outFn);
  }
  var agg = {};
  for (var i = 0; i < linesIn.length; ++i) {
    var line = linesIn[i];
    if (!agg[line]) {
      agg[line] = 1;
    } else {
      agg[line] += 1;
    }
    if (line % 1000000 === 0) {
      console.log('processed line',i,'of',linesIn.length,i/linesIn.length+'%'); // DONOTCHECKIN
    }
  }
  console.log('agg',agg[linesIn[0]]);
  var linesOut = [];
  var numKeys = Object.keys(agg).length;
  for (var k in agg) {
    var freq = agg[k];
    linesOut.push(k+' '+freq);    
  }
  console.log('writing',linesOut[0]); // DONOTCHECKIN
  fs.writeFileSync(outFn, linesOut.join('\n'), {encoding:'utf8'});
}

main(process.cwd(), process.argv);
