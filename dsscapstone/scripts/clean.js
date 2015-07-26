var fs = require('fs');
var path = require('path');

function cleanLine(line) {
  var cl = line.toLowerCase();
  // only ascii at the moment and some extended
  cl = cl.replace(/[^\x00-\xFF]/g, "");
  // remove punctuation we don't care about
  cl = cl.replace(/["'-\/#$%\^&\*{}=\-_`~()<>]/g,"");
  // turn tuple-ending punctuation into a period with a space
  cl = cl.replace(/[\.,!?;:]/g,' . ');
  // standardize whitespace so we don't think about it
  cl = cl.replace(/\s/g,' ');
  return cl;
}

function main(dir, argv) {
  var fn = argv[2];
  var outdir = argv[3];
  if (argv.length !== 4 || !fn || !outdir) {
    console.log('not enough args: <infile> <outdir> or not args',argv.length, fn,outdir); // DONOTCHECKIN
    return;
  }
  if (!fs.existsSync(outdir)) {
    console.error('outdir does not exist',outdir);
    return;
  }
  var p = path.parse(fn);
  var outFn = path.join(outdir, p.name+'_clean'+p.ext);
  console.log('cleaning\tsrc',fn,'outdir',outdir);
  lines = fs.readFileSync(fn, {encoding: 'utf8'}).split('\n');
  console.log('done reading\tline count',lines.length); // DONOTCHECKIN
  var cleanLines = lines.map(function(line) { return cleanLine(line); });
  console.log('processed\tline count', cleanLines.length);
  fs.writeFile(outFn,cleanLines.join('\n'), {encoding:'utf8'}); 
}

main(process.cwd(), process.argv);
