# Change Log

## [0.4.6] - 2020-08-09
### Changed
- Documentation improvement and dependency bump

## [0.4.5] - 2020-08-09
### Fixed
- Fix suboptimality for vectors and lists too

## [0.4.4] - 2020-08-08
### Fixed
- Fix A\* optimality for special cases of smaller `a`

## [0.4.3] - 2020-04-29
### Changed
- Change A\* algorithm equality handling to improve speed for very small diffs

## [0.4.2] - 2019-09-24
### Changed
- Change equality handling to accommodate older versions of Clojure (1.9.0 and older)

## [0.4.1] - 2019-09-20
### Fixed
- Relax `valid-edits?` to accept more valid edits

## [0.4.0] - 2019-07-15
### Added
- `edits->script` function to convert a vector of edits to an EditScript
- `valid-edits?` function to validate the edits vector
- link to cljdoc documentation
### Changed
- Instead of using cost, use a more accurate size for `get-size` of an EditScript

## [0.3.3] - 2019-04-04
### Changed
- Minor dependency bump

## [0.3.2] - 2018-05-31
### Added
- `combine` function to combine two EditScripts
- package.json for npm publising [Andrea Richiardi]
### Fixed
- Fix a cljs warning
- Minor speed improvement

## [0.3.1] - 2018-05-09
### Fixed
- Revert heuristic change in 0.3.0, which breaks optimality

## [0.3.0] - 2018-05-09
### Changed
- cljc version
- Simplify heuristic
- Use defn in place of declare, see http://dev.clojure.org/jira/browse/CLJS-1871

## [0.2.4] - 2018-05-05
### Changed
- Expand the use of quick algorithm in `A*` to cases where one party contains only leaves
- Implements pairing heap for priority queue

### Removed
- java.util.PriorityQueue and HashMap as dependency

### Fixed
- Wrong test ns declaration preventing `lein test`

## [0.2.3] - 2018-05-03
### Changed
- `A*` uses quick algorithm for all leaves list/vector comparison
- Quick algorithm aggressively converts replacement

## [0.2.2] - 2018-05-02
### Changed
- `A*` uses global order number for heuristic

## [0.2.1] - 2018-04-30
### Changed
- Developed an `A*` algorithm for diffing

### Removed
- clojure.data.priority-map as a dependency

### Fixed
- all tests passing

## 0.1.1 - 2018-03-04
### Added
- Initial commits
