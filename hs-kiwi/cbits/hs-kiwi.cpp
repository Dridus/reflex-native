#include <kiwi.h>


// WARNING: keep in sync with Kiwi.Raw.Errors
typedef enum {
  kiwi_errorType_UnsatisfiableConstraint,
  kiwi_errorType_UnknownConstraint,
  kiwi_errorType_DuplicateConstraint,
  kiwi_errorType_UnknownEditVariable,
  kiwi_errorType_DuplicateEditVariable,
  kiwi_errorType_BadRequiredStrength,
  kiwi_errorType_InternalSolverError
} kiwi_errorType;

typedef struct {
  kiwi_errorType type;
  const char* message;
} kiwi_error;

#define catch_to_kiwi_error \
  catch (const kiwi::UnsatisfiableConstraint& e) { return { kiwi_errorType_UnsatisfiableConstraint, NULL }; } \
  catch (const kiwi::UnknownConstraint& e) { return { kiwi_errorType_UnknownConstraint, NULL }; } \
  catch (const kiwi::DuplicateConstraint& e) { return { kiwi_errorType_DuplicateConstraint, NULL }; } \
  catch (const kiwi::UnknownEditVariable& e) { return { kiwi_errorType_UnknownEditVariable, NULL }; } \
  catch (const kiwi::DuplicateEditVariable& e) { return { kiwi_errorType_DuplicateEditVariable, NULL }; } \
  catch (const kiwi::BadRequiredStrength& e) { return { kiwi_errorType_BadRequiredStrength, NULL }; } \
  catch (const kiwi::InternalSolverError& e) { return { kiwi_errorType_InternalSolverError, strdup(e.what()) }; }


kiwi::Constraint* kiwiConstraint_new(const kiwi::Expression* expression, RelationalOperator op, double strength) {
  return new kiwi::Constraint(*expression, op, strength);
}

kiwi::Constraint* kiwiConstraint_copy(const kiwi::Constraint* constraint, double strength) {
  return new kiwi::Constraint(*constraint, strength);
}

void kiwiConstraint_free(const kiwi::Constraint* constraint) {
  delete constraint;
}

kiwi::Expression* kiwiConstraint_getExpression(const kiwi::Constraint* constraint) {
  return new kiwi::Expression(constraint->expression());
}

RelationalOperator kiwiConstraint_getOperator(const kiwi::Constraint* constraint) {
  return constraint->op();
}

double kiwiConstraint_getStrength(const kiwi::Constraint* constraint) {
  return constraint->strength();
}


void kiwiExpression_new_addTerm(std::vector<kiwi::Term>& terms, const kiwi::Term* term) {
  terms.push_back(*term);
}

kiwi::Expression* kiwiExpression_new(void (*fillTerms)(std::vector<kiwi::Term>&, void (*)(std::vector<kiwi::Term>&, const kiwi::Term* term)), double constant) {
  std::vector<kiwi::Term> terms;
  fillTerms(terms, kiwiExpression_new_addTerm);
  return new kiwi::Expression(terms, constant);
}

void kiwiExpression_free(kiwi::Expression* expr) {
  delete expr;
}

const kiwi::Term** kiwiExpression_getTerms(const kiwi::Expression* expr) {
  typedef std::vector<kiwi::Term>::const_iterator iter_t;
  iter_t it = expr->terms().begin();
  iter_t end = expr->terms().end();
  kiwi::Term** ret = new kiwi::Term[expr->terms().size()];
  for (kiwi::Term** p = ret; it != end; ++it) {
    *(p++) = new kiwi::Term(*it);
  }
  return ret;
}

int kiwiExpression_getTermCount(const kiwi::Expression* expr) {
  return expr->terms().size();
}

double kiwiExpression_getConstant(const kiwi::Expression* expr) {
  return expr->constant();
}

double kiwiExpression_getValue(const kiwi::Expression* expr) {
  return expr->value();
}


kiwi::Solver* kiwiSolver_new() {
  return new kiwi::Solver();
}

void kiwiSolver_free(kiwi::Solver* solver) {
  delete solver;
}

kiwi_error* kiwiSolver_addConstraint(kiwi::Solver* solver, const Constraint* constraint) {
  try {
    solver->addConstraint(*constraint);
    return NULL;
  } catch_to_kiwi_error
}

kiwi_error* kiwiSolver_removeConstraint(kiwi::Solver* solver, const Constraint* constraint) {
  try {
    solver->removeConstraint(*constraint);
    return NULL;
  } catch_to_kiwi_error
}

bool kiwiSolver_hasConstraint(kiwi::Solver* solver, const Constraint* constraint) {
  return solver->hasConstraint(*constraint);
}

kiwi_error* kiwiSolver_addEditVariable(kiwi::Solver* solver, const Variable* variable, double strength) {
  try {
    solver->addEditVariable(*variable, strength);
    return NULL;
  } catch_to_kiwi_error
}

kiwi_error* kiwiSolver_removeEditVariable(kiwi::Solver* solver, const Variable* variable) {
  try {
    solver->removeEditVariable(*variable);
    return NULL;
  } catch_to_kiwi_error
}

bool kiwiSolver_hasEditVariable(kiwi::Solver* solver, const Variable* variable) {
  return solver->hasEditVariable(*variable);
}

kiwi_error* kiwiSolver_suggestValue(kiwi::Solver* solver, const Variable* variable, double value) {
  try {
    solver->suggestValue(*variable, value);
    return NULL;
  } catch_to_kiwi_error
}

void kiwiSolver_updateVariables(kiwi::Solver* solver) {
  solver->updateVariables();
}

void kiwiSolver_reset(kiwi::Solver* solver) {
  solver->reset();
}

void kiwiSolver_dump(kiwi::Solver* solver) {
  solver->dump();
}


kiwi::Term* kiwiTerm_new(const kiwi::Variable* variable, double coefficient) {
  return new kiwi::Term(*variable, coefficient);
}

void kiwiTerm_free(const kiwi::Term* term) {
  delete term;
}

const kiwi::Variable* kiwiTerm_getVariable(const kiwi::Term* term) {
  return new kiwi::Variable(term->variable());
}

double kiwiTerm_getCoefficient(const kiwi::Term* term) {
  return term->coefficient();
}

double kiwiTerm_getValue(const kiwi::Term* term) {
  return term->value();
}


kiwi::Variable* kiwiVariable_new(const char* name) {
  return new kiwi::Variable(name);
}

void kiwiVariable_free(kiwi::Variable* var) {
  delete var;
}

const char* kiwiVariable_getName(const kiwi::Variable* var) {
  return var->name().c_str();
}

void kiwiVariable_setName(kiwi::Variable* var, const char* name) {
  var->setName(name);
}

double kiwiVariable_getValue(const kiwi::Variable* var) {
  return var->value();
}

void* kiwiVariable_getIdentity(kiwi::Variable* var) {
  return var->identity();
}

