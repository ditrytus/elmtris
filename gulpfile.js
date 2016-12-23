var gulp = require('gulp'),
    elm = require('gulp-elm'),
    connect = require('gulp-connect'),
    del = require('del'),
    livereload = require('gulp-livereload');

var sourceFolder = 'src/';
var outputFolder = 'dist/'

var elmMainFile = sourceFolder + 'app/Main.elm';

var elmFiles = '**/*.elm';
var cssFiles = '**/*.css';
var htmlFiles = '**/*.html';
var fontFiles = '**/*.woff*';

var elmBundleFile = 'elmtris.js'

var srcElmFiles = sourceFolder + elmFiles;

var srcCssFiles = sourceFolder + cssFiles;
var srcHtmlFiles = sourceFolder + htmlFiles;
var srcFontFiles = sourceFolder + fontFiles;

var srcAssetsFiles = [srcCssFiles, srcHtmlFiles, srcFontFiles];

var outCssFiles = outputFolder + cssFiles;
var outHtmlFiles = outputFolder + htmlFiles;
var outFontFiles = outputFolder + fontFiles;
var outElmBundle = outputFolder + elmBundleFile;

var outFiles = [outCssFiles, outHtmlFiles, outFontFiles, outElmBundle]

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function() {
  return gulp.src(elmMainFile)
    .pipe(elm.bundle(elmBundleFile).on('error', function () {}))
    .pipe(gulp.dest(outputFolder))
    .pipe(livereload());
});

gulp.task('assets', function() {
  return gulp.src(srcAssetsFiles)
    .pipe(gulp.dest(outputFolder))
    .pipe(livereload());
});

gulp.task('clean', function() {
  return del(outFiles);
});

gulp.task('watch', function() {
  livereload.listen();
  gulp.watch(srcElmFiles, ['elm']);
  gulp.watch(srcAssetsFiles, ['assets']);
});

gulp.task('build', ['clean'], function() {
  gulp.start(['elm', 'assets']);
});

gulp.task('connect', function() {
  connect.server({
    root: 'dist',
    livereload: true
  });
});