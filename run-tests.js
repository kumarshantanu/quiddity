try {
  phantom.injectJs('target/quiddity-test.js');
  console.log('Injected target/quiddity-test.js');
  quiddity.run_tests.run();
} catch (e) {
  console.log('Found exception');
  console.log(e);
  console.log(e.fileName);
  console.log(e.trace);
} finally {
  phantom.exit();
}
