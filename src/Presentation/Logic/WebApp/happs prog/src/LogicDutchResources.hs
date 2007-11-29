module LogicDutchResources where
--

-- Used in LogicSolver: ------------------------------------------------ 

-- frame labels
logic_solverText                           =  "Logic Solver"
fileText                                   =  "File"
quit_the_demoText                          =  "Sluit de demo"
about_logic_solverText                     =  "Over de Logic Solver"
welcome_to_Logic_SolverText                =  "Welkom bij Logic Solver"
new_formulaText                            =  "Nieuwe formule"
new_formula_ctrlKey                        =  "&Nieuwe formule\tCtrl+N"
this_is_a_demo_of_the_Logic_SolverText     =  "Dit is een demo van de Logic Solver"

-- Menu pane labels
rewriteRulesText          = "&Herschrijfregels"
showRulesText             = "&Toon regels"
list_of_rewrite_rulesText = "Lijst van herschrijfregels"
list_of_rulesText         = "Implicatie-eliminatie   \t f1 -> f2    \t <=> (~f1) \\/ f2 \n"                          ++
                            "Equivalentie-eliminatie \t f1 <-> f2   \t <=> (f1 /\\ f2) \\/ ((~f1) /\\ (~f2)) \n"     ++
                            "De Morgan               \t ~(f1 /\\ f2)\t <=> ~f1) \\/ ~f2 \n"                          ++
                            "De Morgan               \t ~(f1 \\/ f2)\t <=> ~f1 /\\  ~f2 \n"                          ++
                            "Dubbele negatie         \t ~~f         \t <=> f \n"                                     ++                                         "Distributiviteit        \t\t f1 /\\ (f2 \\/ f3)) \t <=> (f1 /\\ f2) \\/ (f1 /\\ f3) \n" ++
                            "Distributiviteit        \t\t f1 \\/ (f2 /\\ f3)) \t <=> (f1 \\/ f2) /\\ (f1 \\/ f3) \n" ++
                            "Idempotentie            \t f1 /\\ f1  \t\t <=> f1 \n"                                   ++
                            "Idempotentie            \t f1 \\/ f1  \t\t <=> f1 \n"                                   ++
                            "Associativiteit         \t\t (f1 /\\ f2) /\\ f3 \t <=> f1 /\\ (f2 /\\ f3) \n"           ++
                            "Associativiteit         \t\t (f1 \\/ f2) \\/ f3 \t <=> f1 \\/ (f2 \\/ f3) \n"           ++
                            "commutativiteit         \t f1 /\\ f2 \t\t <=> f2 /\\ f1 \n"                             ++
                            "commutativiteit         \t f1 \\/ f2 \t\t <=> f2 \\/ f1 \n"                             ++
                            "False-True              \t\t T \\/  f \t\t <=> T \n"                                   ++
                            "False-True              \t\t T /\\  f \t\t <=> f \n"                                   ++  
                            "False-True              \t\t F \\/  f \t\t <=> f \n"                                   ++
                            "False-True              \t\t F /\\  f \t\t <=> F \n"                                   ++ 
                            "False-True              \t\t f \\/ ~f \t\t <=> T \n"                                  ++ 
                            "False-True              \t\t f /\\ ~f \t\t <=> F \n"                                   ++
                            "False-True              \t\t ~T       \t\t <=> F \n"                                   ++
                            "False-True              \t\t ~F       \t\t <=> T \n"


-- button texts
submitText        = "Submit"
undoText          = "Herstel"
hintText          = "Hint"
nextStepText      = "Volgende stap"
solvedText        = "Klaar"

-- labels for the text fields
working_areaText  = "Werkveld"
historyText       = "Historie"
feedbackText      = "Feedback"

-- Used in LogicFeedback: ----------------------------------------------- 
is_SolvedText                        = "De formule staat inderdaad in dnf"
not_solvedText                       = "De formule staat nog niet in dnf"
formula_was_already_solved           = "Je hebt de formule in de vorige stap al in dnf geschreven"
formula_syntactically_incorrectText  = "De ingevoerde formule is syntactisch niet correct." 
correctly_rewritten_butText          = "Je hebt de formule correct herschreven, maar je hebt:"
unknown_rewrite_ruleText             = "  - een onbekende herschrijfregel gebruikt;"
sequence_of_rewrite_rulesText        = "  - of een aantal herschrijfregels na elkaar gebruikt i.p.v. een herschrijfregel."
correctly_rewrittenText              = "Je hebt de formule correct herschreven in een dnf."

-- ===========================================================================================================================
-- equivalent_butText                   = "Deze formule is een correcte herschrijving van de vorige formule,"
-- not_possible_to_rewriteText          = "maar het is niet mogelijk de vorige formule te herschrijven"
-- into_this_formulaText                = "tot deze formule wanneer alleen de gegeven herschrijfregels gebruikt mogen worden."
-- ===========================================================================================================================

equivalent_butText                   = "Deze formule is logisch equivalent met de vorige formule,"
not_possible_to_rewriteText          = "maar omdat u per stap slechts een herschrijfregel uit mag voeren, accepteert de solver uw formule niet."
into_this_formulaText                = "Druk op herstel of wijzig de formule."


no_differenceText                    = "Er is geen verschil tussen de oude en de nieuwe formule."
you_have_rewrittenText               = "Je hebt het volgende gedeelte herschreven: "
in_Text                              = "in"
dotText                              = "."
this_rewritting_is_incorrectText     = "Deze herschrijving is niet correct."
no_rule_detectedText                 = "Er is geen herschrijfregel gedetecteerd die je waarschijnlijk hebt toegepast."
press_undoText                       = "Druk op de Herstel-knop en/of wijzig de formule!"
you_have_tried_to_applyText          = "Je hebt geprobeerd toe te passen de "
on_the_partText                      = "op het gedeelte:"
incorrect_truefalseText              = "Deze herschrijving is niet juist"
however_implicationText              = "Echter, de implicatie operator (->) is niet commutatief."
however_equivalenceText              = "Echter, de equivalentie operator (<->) is niet commutatief."
incorrect_removal_of_negationsText   = "Deze eliminatie van negaties is niet juist."
implication_is_not_associativeText   = "Een implicatie operator (->) is niet associatief."
incorrectly_appliedText              = "Je hebt onjuist toegepast de  "
correct_application_resultsText      = "Correcte toepassing van de regel resulteert in:"
empty_stringText                     = ""

truefalseText                        = "true-false regel"
elimination_implicationText          = "implicatie eliminatie regel"
elimination_equivalenceText          = "equivalentie eliminatie regel"
de_Morgan_ruleText                   = "De Morgan regel"
remove_double_negationsText          = "eliminatie van een dubbele negatie regel"
distribute_and_over_orText           = "distributie van 'en' over 'of' regel"
commutativity_ruleText               = "commutativitiet regel"
associativity_ruleText               = "associativiteit regel"

no_diffText                          = "Geen verschil ...."
or_signText                          = "|"

-- for hints 
you_can_simplify_the_formula_using_true_false_ruleText
                                         = "Je kunt de formule vereenvoudigen door een true-false regel toe te passen."
you_can_eliminate_an_implicationText     = "Je kan een implicatie elimineren."
you_can_eliminate_an_equivalanceText     = "Je kan een equivalentie elimineren."
you_can_apply_De_Morgan_ruleText         = "Je kan de De Morgan regel toepassen." 
you_can_eliminate_a_double_negationText  = "Je kan een dubbele negatie elimineren." 
you_can_distribute_and_over_orText       = "Je kan 'and' over 'or' distribueren." 
you_have_already_solved_the_exerciseText = "Je hebt de opgave al opgelost."  
no_hint_availableText                    = "Er is geen hint beschikbaar"

-- for the indicators
progressText                         = "Voortgang:"
the_number_of_implicationsText       = "Het aantal te elimineren implicaties: "
the_number_of_equivalencesText       = "Het aantal te elimineren equivalences: "
the_number_of_double_negationsText   = "Het aantal te elimineren dubbele negaties: "

-- for next step
you_can_simplify_the_formulaText          = "Je kan de volgende formule vereenvoudigen"
using_a_true_false_ruleText               = "door een true-false regel toe te passen"
you_can_eliminate_the_implicationText     = "Je kan de volgende implicatie elimineren: "
you_can_eliminate_the_equivalanceText     = "Je kan de volgende equivalentie elimineren: "  
you_can_apply_De_Morgan_rule_onText       = "Je kan de De Morgan regel toepassen op: " 
you_can_eliminate_the_double_negationText = "Je kan de volgende dubbele negatie elimineren: " 
you_can_distribute_and_over_or_inText     = "Je kan 'and' over 'or' distribueren in: "

-- Used in LogicParser: 
error_in_lineText                = "Er is een syntaxfout geconstateerd"
did_you_maybe_meanText           = "Bedoelt u soms: "
which_results_inText             = "wat resulteert in: "
