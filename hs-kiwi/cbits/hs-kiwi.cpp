#include <string>
#include <kiwi.h>


extern "C" kiwi::ErrorType kiwiError_getType(kiwi::Error* err) {
  return err->type();
}

extern "C" const char* kiwiError_getMessage(kiwi::Error* err) {
  return err->what();
}

extern "C" void kiwiError_free(kiwi::Error* err) {
  delete err;
}

extern "C" kiwi::Constraint* kiwiConstraint_new(const kiwi::Expression* expression, kiwi::RelationalOperator op, double strength) {
  return new kiwi::Constraint(*expression, op, strength);
}

extern "C" kiwi::Constraint* kiwiConstraint_copy(const kiwi::Constraint* constraint, double strength) {
  return new kiwi::Constraint(*constraint, strength);
}

extern "C" void kiwiConstraint_free(const kiwi::Constraint* constraint) {
  delete constraint;
}

extern "C" kiwi::Expression* kiwiConstraint_getExpression(const kiwi::Constraint* constraint) {
  return new kiwi::Expression(constraint->expression());
}

extern "C" kiwi::RelationalOperator kiwiConstraint_getOperator(const kiwi::Constraint* constraint) {
  return constraint->op();
}

extern "C" double kiwiConstraint_getStrength(const kiwi::Constraint* constraint) {
  return constraint->strength();
}


extern "C" void kiwiExpression_new_addTerm(std::vector<kiwi::Term>& terms, const kiwi::Term* term) {
  terms.push_back(*term);
}

extern "C" kiwi::Expression* kiwiExpression_new(void (*fillTerms)(std::vector<kiwi::Term>&, void (*)(std::vector<kiwi::Term>&, const kiwi::Term* term)), double constant) {
  std::vector<kiwi::Term> terms;
  fillTerms(terms, kiwiExpression_new_addTerm);
  return new kiwi::Expression(terms, constant);
}

extern "C" void kiwiExpression_free(kiwi::Expression* expr) {
  delete expr;
}

extern "C" kiwi::Term** kiwiExpression_getTerms(const kiwi::Expression* expr) {
  typedef std::vector<kiwi::Term>::const_iterator iter_t;
  iter_t it = expr->terms().begin();
  iter_t end = expr->terms().end();
  kiwi::Term** ret = new kiwi::Term*[expr->terms().size()];
  for (kiwi::Term** p = ret; it != end; ++it) {
    *(p++) = new kiwi::Term(*it);
  }
  return ret;
}

extern "C" int kiwiExpression_getTermCount(const kiwi::Expression* expr) {
  return expr->terms().size();
}

extern "C" double kiwiExpression_getConstant(const kiwi::Expression* expr) {
  return expr->constant();
}

extern "C" double kiwiExpression_getValue(const kiwi::Expression* expr) {
  return expr->value();
}


extern "C" kiwi::Solver* kiwiSolver_new() {
  return new kiwi::Solver();
}

extern "C" void kiwiSolver_free(kiwi::Solver* solver) {
  delete solver;
}

extern "C" kiwi::Error* kiwiSolver_addConstraint(kiwi::Solver* solver, const kiwi::Constraint* constraint) {
  return solver->addConstraint(*constraint);
}

extern "C" kiwi::Error* kiwiSolver_removeConstraint(kiwi::Solver* solver, const kiwi::Constraint* constraint) {
  return solver->removeConstraint(*constraint);
}

extern "C" int kiwiSolver_hasConstraint(kiwi::Solver* solver, const kiwi::Constraint* constraint) {
  return solver->hasConstraint(*constraint) ? 1 : 0;
}

extern "C" kiwi::Error* kiwiSolver_addEditVariable(kiwi::Solver* solver, const kiwi::Variable* variable, double strength) {
  return solver->addEditVariable(*variable, strength);
}

extern "C" kiwi::Error* kiwiSolver_removeEditVariable(kiwi::Solver* solver, const kiwi::Variable* variable) {
  return solver->removeEditVariable(*variable);
}

extern "C" int kiwiSolver_hasEditVariable(kiwi::Solver* solver, const kiwi::Variable* variable) {
  return solver->hasEditVariable(*variable) ? 1 : 0;
}

extern "C" kiwi::Error* kiwiSolver_suggestValue(kiwi::Solver* solver, const kiwi::Variable* variable, double value) {
  return solver->suggestValue(*variable, value);
}

extern "C" void kiwiSolver_updateVariables(kiwi::Solver* solver) {
  solver->updateVariables();
}

extern "C" void kiwiSolver_reset(kiwi::Solver* solver) {
  solver->reset();
}

extern "C" void kiwiSolver_dump(kiwi::Solver* solver) {
  solver->dump();
}


extern "C" kiwi::Term* kiwiTerm_new(const kiwi::Variable* variable, double coefficient) {
  return new kiwi::Term(*variable, coefficient);
}

extern "C" void kiwiTerm_free(const kiwi::Term* term) {
  delete term;
}

extern "C" const kiwi::Variable* kiwiTerm_getVariable(const kiwi::Term* term) {
  return new kiwi::Variable(term->variable());
}

extern "C" double kiwiTerm_getCoefficient(const kiwi::Term* term) {
  return term->coefficient();
}

extern "C" double kiwiTerm_getValue(const kiwi::Term* term) {
  return term->value();
}


extern "C" kiwi::Variable* kiwiVariable_new(const char* name) {
  return new kiwi::Variable(name);
}

extern "C" void kiwiVariable_free(kiwi::Variable* var) {
  delete var;
}

extern "C" const char* kiwiVariable_getName(const kiwi::Variable* var) {
  return var->name().c_str();
}

extern "C" void kiwiVariable_setName(kiwi::Variable* var, const char* name) {
  var->setName(name);
}

extern "C" double kiwiVariable_getValue(const kiwi::Variable* var) {
  return var->value();
}

extern "C" const void* kiwiVariable_getIdentity(kiwi::Variable* var) {
  return var->identity();
}

