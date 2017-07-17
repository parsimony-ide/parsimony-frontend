(ns parsimony.test-runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [parsimony.asm-inference-test]
            [parsimony.asm-parser-test]
            [parsimony.dag-test]
            [parsimony.heuristic-test]
            [parsimony.inference-test]
            [parsimony.lexer-test]
            [parsimony.overlay-test]
            [parsimony.parser-performance-test]
            [parsimony.parser-test]
            [parsimony.refactor-test]
            [parsimony.solver-impl-test]
            [parsimony.transit-test]
            [parsimony.union-find-test]
            [parsimony.util-test]))

(doo-tests 'parsimony.asm-inference-test
           'parsimony.asm-parser-test
           'parsimony.dag-test
           'parsimony.heuristic-test
           'parsimony.inference-test
           'parsimony.lexer-test
           'parsimony.overlay-test
           'parsimony.parser-performance-test
           'parsimony.parser-test
           'parsimony.refactor-test
           'parsimony.solver-impl-test
           'parsimony.transit-test
           'parsimony.union-find-test
           'parsimony.util-test)
