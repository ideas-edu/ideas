module LogicEnglishResources where


-- Used in LogicSolver: ------------------------------------------------ 

-- frame labels
logic_solverText                           =  "Logic Solver"
fileText                                   =  "File"
quit_the_demoText                          =  "Quit the demo"
about_logic_solverText                     =  "About Logic Solver"
welcome_to_Logic_SolverText                =  "Welcome to Logic Solver"
new_formulaText                            =  "New formula"
new_formula_ctrlKey                        = "&Nieuwe formule\tCtrl+N"
this_is_a_demo_of_the_Logic_SolverText     =  "This is a demo of the Logic Solver"

-- Menu pane labels
rewriteRulesText          = "&Rewrite rules"
showRulesText             = "&Show rules"
list_of_rewrite_rulesText = "List of rewrite rules"
list_of_rulesText         = "Implication-elimination   \t f1 -> f2    \t <=> (~f1) \\/ f2 \n"                        ++
                            "Equivalency-elimination   \t f1 <-> f2   \t <=> (f1 /\\ f2) \\/ ((~f1) /\\ (~f2)) \n"   ++
                            "De Morgan                 \t ~(f1 /\\ f2)\t <=> ~f1) \\/ ~f2 \n"                        ++
                            "De Morgan                 \t ~(f1 \\/ f2)\t <=> ~f1 /\\  ~f2 \n"                        ++
                            "Double negation           \t ~~f         \t <=> f \n"                                   ++
                            "Distribution              \t f1 /\\ (f2 \\/ f3)) \t <=> (f1 /\\ f2) \\/ (f1 /\\ f3) \n" ++
                            "Distribution              \t f1 \\/ (f2 /\\ f3)) \t <=> (f1 \\/ f2) /\\ (f1 \\/ f3) \n" ++
                            "Idempotency               \t f1 /\\ f1  \t\t <=> f1 \n"                                 ++
                            "Idempotency               \t f1 \\/ f1  \t\t <=> f1 \n"                                 ++
                            "Associativity             \t (f1 /\\ f2) /\\ f3 \t <=> f1 /\\ (f2 /\\ f3) \n"           ++
                            "Associativity             \t (f1 \\/ f2) \\/ f3 \t <=> f1 \\/ (f2 \\/ f3) \n"           ++
                            "Communativity             \t f1 /\\ f2 \t\t <=> f2 /\\ f1 \n"                           ++
                            "Communativity             \t f1 \\/ f2 \t\t <=> f2 \\/ f1 \n"                           ++
                            "False-True                \t\t T \\/  f) \t\t <=> T \n"                                 ++
                            "False-True                \t\t T /\\  f) \t\t <=> f \n"                                 ++  
                            "False-True                \t\t F \\/  f) \t\t <=> f \n"                                 ++
                            "False-True                \t\t F /\\  f) \t\t <=> F \n"                                 ++ 
                            "False-True                \t\t f \\/ ~f) \t\t <=> T \n"                                 ++ 
                            "False-True                \t\t f /\\ ~f) \t\t <=> F \n"

-- button texts
submitText        = "Submit"
undoText          = "Undo"
hintText          = "Hint"
nextStepText      = "Next step"
solvedText        = "Solved"

-- labels for the text fields
working_areaText  = "Working area"
historyText       = "History"
feedbackText      = "Feedback"


-- Used in LogicFeedback: ----------------------------------------------- 
is_SolvedText                        = "You have reached a dnf"
not_solvedText                       = "You have not yet reached a dnf"
formula_was_already_solved           = "You have already reached a dnf in the previous step" 
formula_syntactically_incorrectText  = "The formula you have entered is syntactically incorrect." 
correctly_rewritten_butText          = "You have correctly rewritten the formula, but you have applied:"
unknown_rewrite_ruleText             = "  - an unknown rewrite rule;"
sequence_of_rewrite_rulesText        = "  - or a sequence of rewrite rules instead of one rewrite rule."
correctly_rewrittenText              = "You have correctly rewritten the formula in a dnf."
equivalent_butText                   = "This formula is equivalent to the previous one."
not_possible_to_rewriteText          = "But it is not possible to rewrite the previous formula"
into_this_formulaText                = "into this formula using a single rule given the set of rewrite rules."
no_differenceText                    = "There is no difference between the old and the submitted formula."
you_have_rewrittenText               = "You have rewritten the part:"
in_Text                              = "in"
dotText                              = "."
this_rewritting_is_incorrectText     = "This rewritting is incorrect."
no_rule_detectedText                 = "There is no rule detected which you have probably applied."
-- press_undoText                       = "Press the undo button and try again!!"
you_have_tried_to_applyText          = "You have tried to apply the"
on_the_partText                      = "on the part:"
incorrect_truefalseText              = "This rewritting is incorrect."
however_implicationText              = "However, the implication operator (->) is not commutative."
however_equivalenceText              = "However, the equivalence operator (<->) is not commutative."
incorrect_removal_of_negationsText   = "This removal of negations is incorrect."
implication_is_not_associativeText   = "An implication is not associative."
incorrectly_appliedText              = "You have incorrectly applied the "
correct_application_resultsText      = "Correct application of the rule results in:"
empty_stringText                     = ""

truefalseText                        = "true-false rule"
elimination_implicationText          = "elimination of an implication rule"
elimination_equivalenceText          = "elimination of an equivalence rule"
de_Morgan_ruleText                   = "De Morgan rule"
remove_double_negationsText          = "remove double negations rule"
distribute_and_over_orText           = "distribute 'and' over 'or' rule"
commutativity_ruleText               = "commutativity rule"
associativity_ruleText               = "associativity rule"

no_diffText                          = "No diff...."
or_signText                          = "|"


-- for hints 
you_can_simplify_the_formula_using_true_false_ruleText = "You can simplify the formula using a true-false rule"
you_can_eliminate_an_implicationText     = "You can eliminate an implication."
you_can_eliminate_an_equivalanceText     = "You can eliminate an equivalance."
you_can_apply_De_Morgan_ruleText         = "You can apply De Morgan rule." 
you_can_eliminate_a_double_negationText  = "You can eliminate a double negation." 
you_can_distribute_and_over_orText       = "You can distribute 'and' over 'or'." 
you_have_already_solved_the_exerciseText = "You have already solved the exercise."  
no_hint_availableText                    = "There is no hint available"

-- for the indicators
progressText                         =  "Progress:"
the_number_of_implicationsText       = "The number of implications to eliminate: "
the_number_of_equivalencesText       = "The number of equivalences to eliminate: "
the_number_of_double_negationsText   = "The number of double negations to eliminate: "

-- for next step
you_can_simplify_the_formulaText          = "You can simplify the formule"
using_a_true_false_ruleText               = "using a true-false rule"
you_can_eliminate_the_implicationText     = "You can eliminate the implication: "
you_can_eliminate_the_equivalanceText     = "You can eliminate the equivalance: "  
you_can_apply_De_Morgan_rule_onText       = "You can apply De Morgan rule on: " 
you_can_eliminate_the_double_negationText = "You can eliminate the double negation: " 
you_can_distribute_and_over_or_inText     = "You can distribute 'and' over 'or' in: "


-- Used in LogicParser: 
error_in_lineText                = "Error in line"
did_you_maybe_meanText           = "Did you maybe mean:"
which_results_inText             = "which results in: "
