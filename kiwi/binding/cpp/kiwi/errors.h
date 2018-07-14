/*-----------------------------------------------------------------------------
| Copyright (c) 2013-2017, Nucleic Development Team.
|
| Distributed under the terms of the Modified BSD License.
|
| The full license is in the file COPYING.txt, distributed with this software.
|----------------------------------------------------------------------------*/
#pragma once
#include <exception>
#include <string>
#include "constraint.h"
#include "variable.h"


namespace kiwi
{

typedef enum {
  UnsatisfiableConstraint,
  UnknownConstraint,
  DuplicateConstraint,
  UnknownEditVariable,
  DuplicateEditVariable,
  BadRequiredStrength,
  InternalSolverError
} ErrorType;

class Error
{
public:
  Error(ErrorType type) : m_errorType(type), m_msg("an error occurred") {}
  Error(ErrorType type, const char* msg) : m_errorType(type), m_msg(msg) {}
  Error(ErrorType type, const std::string& msg) : m_errorType(type), m_msg(msg) {}

  ErrorType type() const throw() {
    return m_errorType;
  }

  const char* what() const throw() {
    return m_msg.c_str();
  }

private:
  ErrorType m_errorType;
  std::string m_msg;
};

} // namespace kiwi
