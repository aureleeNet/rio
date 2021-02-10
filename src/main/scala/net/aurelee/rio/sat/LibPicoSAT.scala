package net.aurelee.rio.sat

import com.sun.jna.Library

trait LibPicoSAT extends Library {
  def picosat_init() : Long
  def picosat_enable_trace_generation(context: Long) : Int
  def picosat_reset(context: Long) : Unit
  def picosat_res(context: Long) : Int
  def picosat_inc_max_var(context: Long) : Int
  def picosat_add(context: Long, lit: Int) : Int
  def picosat_sat(context: Long, decision_limit: Int) : Int
  def picosat_set_global_default_phase(context: Long, phase: Int) : Unit
  def picosat_set_default_phase_lit(context: Long, lit: Int, phase: Int) : Unit
  def picosat_reset_phases(context: Long) : Unit
  def picosat_reset_scores(context: Long) : Unit
  def picosat_remove_learned(context: Long, percentage: Int) : Unit
  def picosat_set_more_important_lit(context: Long, lit: Int) : Unit
  def picosat_set_less_important_lit(context: Long, lit: Int) : Unit
  def picosat_adjust(context: Long, maxIdx : Int) : Unit
  def picosat_variables(context: Long) : Int
  def picosat_added_original_clauses(context: Long) : Int
  def picosat_seconds(context: Long) : Double
  def picosat_assume(context: Long, lit: Int) : Unit
  def picosat_deref(context: Long, lit: Int) : Int
  def picosat_deref_toplevel(context: Long, lit: Int) : Int
  def picosat_inconsistent(context: Long) : Int
  def picosat_failed_assumption(context: Long, lit: Int) : Int
  def picosat_failed_assumptions(context: Long) : List[Int]
  def picosat_changed(context: Long) : Int
  def picosat_coreclause(context: Long, clauseIdx: Int) : Int
  def picosat_corelit(context: Long, lit: Int) : Int
  def picosat_usedlit(context: Long, lit: Int) : Int
  def picosat_version(): String
}
