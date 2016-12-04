var gulp = require('gulp'),
    elm = require('gulp-elm'),
    connect = require('gulp-connect'),
    del = require('del'),
    livereload = require('gulp-livereload');

var sourceFolder = 'src/';
var outputFolder = 'dist/'
var elmMainFile = sourceFolder + 'app/Main.elm';
var elmFiles = sourceFolder + '**/*.elm';
var cssFiles = sourceFolder + '**/*.css';
var htmlFiles = sourceFolder + '**/*.html';
var fontFiles = sourceFolder + '**/*.woff*';
var assetsFiles = [cssFiles, htmlFiles, fontFiles];

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function() {
  return gulp.src(elmMainFile)
    .pipe(elm.bundle('elmtris.js'))
    .pipe(gulp.dest(outputFolder))
    .pipe(livereload());
});

gulp.task('assets', function() {
  return gulp.src(assetsFiles)
    .pipe(gulp.dest(outputFolder))
    .pipe(livereload());
});

gulp.task('clean', function() {
  return del([outputFolder]);
});

gulp.task('watch', function() {
  livereload.listen();
  gulp.watch(elmFiles, ['elm']);
  gulp.watch(assetsFiles, ['assets']);
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