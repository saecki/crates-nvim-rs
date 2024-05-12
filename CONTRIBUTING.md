# Contributing

## Testing

### Snapshot tests
The [`toml-test-data`](https://crates.io/crates/toml-test-data) crate provides a set of invalid test cases.
Error messages are stored in the `crates-toml/tests/fixtures` directory and compared in the test suite.
If an error message has changed or a new test case was added, the new output can be reviewed and stored by running:
```
SNAPSHOT=review cargo test
```
Possible values for the `SNAPSHOT` mode environment variable are:
- `fail` (default) to just fail the test with an error message.
- `skip` to skip all tests
- `skip-missing` to skip tests with missing fixtures, but still run all others.
- `review` to review each change individually and decide whether to update the output fixture.
- `update` to only review changed fixtures, not missing ones.
- `revise` to review correct fixtures and decide whether to invalidate (remove) them.
- `force` to update all output fixtures

Additionally a filter can be specified like this:
```
SNAPSHOT="review:string" cargo test
```
This filter is used in a simple substring search on the test case file paths.
