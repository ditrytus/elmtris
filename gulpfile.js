var gulp = require('gulp'),
    elm = require('gulp-elm'),
    del = require('del');

var sourceFolder = 'src/';
var outputFolder = 'dist/'
var elmMainFile = sourceFolder + 'app/Main.elm';
var cssFiles = sourceFolder + '**/*.css';
var htmlFiles = sourceFolder + '**/*.html';
var fontFiles = sourceFolder + '**/*.woff*';
var assetsFiles = [cssFiles, htmlFiles, fontFiles];

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function() {
  return gulp.src(elmMainFile)
    .pipe(elm.bundle('elmtris.js'))
    .pipe(gulp.dest(outputFolder))
});

gulp.task('assets', function() {
    return gulp.src([cssFiles, htmlFiles, fontFiles])
      .pipe(gulp.dest(outputFolder));
});

gulp.task('clean', function() {
  return del([outputFolder]);
});

gulp.task('build', ['clean'], function() {
  gulp.start(['elm', 'assets']);
});