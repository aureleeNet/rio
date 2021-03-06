package net.aurelee.rio.sat

/**
  * Represents a stateful PicoSAT context.
  *
  * Objects of this class represent independent PicoSAT contexts. Since PicoSAT is an iterative solver the
  * contexts are stateful. After solving additional clauses can be added.
  * Most of the methods of this object map directly to functions of the PicoSAT C API. See the picosat.h file
  * for an exact documentation of the function behavior. Functions starting with '''picosat_''' are the
  * internally used native functions and should not be used by users of this API. They are currently exposed,
  * because some Scala versions do not support private native functions.
  *
  * @author Hans-Jörg Schurr
 *  @note Ported to JNA library by Alexander Steen on 2021
  * @since 7/27/16.
  */
sealed abstract class PicoSAT(enableTracing: Boolean)  {
  import com.sun.jna.Native
  private[this] final val lib = {
    Native.extractFromResourcePath("picosat")
    Native.load("picosat", classOf[LibPicoSAT])
  }

  import PicoSAT.State

  private[this] var context: Long = lib.picosat_init()
  if(context == 0)
    throw new OutOfMemoryError()

  private[this] val tracing: Boolean = if(enableTracing)
    if (lib.picosat_enable_trace_generation(context) != 0)
      true
    else throw new UnsupportedOperationException("Trace generation not supported.")
  else false

  /**
    * @return Returns the last result of calling [[PicoSAT.solve()]] or [[PicoSAT.Unknown]] if not yet called.
    */
  def state: State = {
    State(lib.picosat_res(context))
  }

  /**
    * Resets the solver contexts by deleting the current one and creating a new one. If tracing was previously enabled
    * it will be enabled again.
    */
  def reset(): Unit = {
    lib.picosat_reset(context)
    context = lib.picosat_init()
    if(tracing) lib.picosat_enable_trace_generation(context)
  }

  /** Allocates a new and unused variable. For subsequent calls to functions on this context this variable
    * is threaten as if it has been used.
    *
    * @return Returns a new unused variable.
    */
  def freshVariable: Int = {
    lib.picosat_inc_max_var(context)
  }

  /** Adds a new clause to the context. A clause consists of a list of integer values representing literals. A negative
    * value represents the negated variable. It is not necessary to use [[PicoSAT.freshVariable]] to allocate a variable
    * first. Adding a clause resets the assignment and state.
    */
  def addClause(lits:Iterable[Int]) : Int = {
    lits.foreach(lib.picosat_add(context,_))
    lib.picosat_add(context, 0)
  }
  def addClause(lits:Int*) : Int = {
    addClause(lits)
  }

  /**
    * Sets the default initial phase.
    *
    * Sets the assignment chosen for a variable initially.
    * After a variable has been assigned the first time, it will always
    * be assigned the previous value if it is picked as decision variable.
    *
    * @param phase  0 = false
    *               1 = true
    *               2 = Jeroslow-Wang (default)
    *               3 = random initial phase
    */
  def setGlobalDefaultPhase(phase: Int): Unit = {
    lib.picosat_set_global_default_phase(context, phase)
  }

  /**
    * Sets the next or initial phase of a variable if picked as a decision variable.
    *
    * @param lit The literal.
    * @param phase negative = next value if picked as decision variable is false
    *              positive = next value if picked as decision variable is true
    *              0        = use global default phase as next value and
    *                         assume 'lit' was never assigned
    */
  def setDefaultPhase(lit: Int, phase: Int): Unit = {
    lib.picosat_set_default_phase_lit(context, lit, phase)
  }

  /** Solves the current context.
    * @return The result of attempting to solve the context.
    */
  def solve() : State = {
    State(lib.picosat_sat(context, -1))
  }

  /**
    * Resets all phases.
    */
  def resetPhases(): Unit = {
    lib.picosat_reset_phases(context)
  }

  /**
    * Resets all scores.
    *
    * Resetting scores and phases does not delete learned clauses or move head tail pointers.
    */
  def resetScores(): Unit = {
    lib.picosat_reset_scores(context)
  }

  /**
    * Deletes a given percentage of large learned clauses.
    *
    * If the current status  is SAT the assignment is reset first. If the given percentage is 100% all large learned
    * clauses are deleted.
    *
    * @param percentage The percentage of clauses to delete.
    */
  def removeLearnedClauses(percentage: Int): Unit = {
    lib.picosat_remove_learned(context, percentage)
  }

  /**
    * Marks a variable as more important.
    *
    * Variables marked as more important are used as decision variables first.
    *
    * @param lit The variable to to mark as more important.
    */
  def setMoreImportant(lit: Int): Unit = {
    lib.picosat_set_more_important_lit(context, lit)
  }

  /**
    * Marks a variable as less important.
    *
    * Variables marked as less important are used as decision variables last.
    *
    * @param lit The variable to to mark as less important.
    */
  def setLessImportant(lit: Int): Unit = {
    lib.picosat_set_less_important_lit(context, lit)
  }

  /**
    * Pre allocate space for '''maxIdx''' variables.
    *
    * This can be used as a small optimization. Allocates variables this function behaves
    * like [[PicoSAT.freshVariable]].
    *
    * @param maxIdx Estimate for the highest variable index appearing in the CNF.
   */
  def adjust(maxIdx : Int): Unit = {
    lib.picosat_adjust(context, maxIdx)
  }

  /** Number of used variables. */
  def numVariables: Int = {
    lib.picosat_variables(context)
  }

  /** Number of user added clauses. */
  def numAddedClauses: Int = {
    lib.picosat_added_original_clauses(context)
  }

  /**
    * @return The total time spend by PicoSAT in calls to [[PicoSAT.solve()]] in seconds.
    */
  def timeSpendSolving: Double = {
    lib.picosat_seconds(context)
  }


  /**
    * Returns a satisfying variable assignment.
    *
    * If the state is SAT this function will return the value assigned to a variable in the found satisfying
    * assignment, or '''None''' if the assignment is unknown (don't care). If the stat is not not SAT '''None''' is
    * returned.
    *
    * @param lit The variable the assignment is for.
    * @return A satisfying assignment or None.
    */
  def getAssignment(lit: Int) : Option[Boolean] = {
    if(state != PicoSAT.SAT)
      None
    else {
      lib.picosat_deref(context, lit) match {
         case v if v > 0 => Some(true)
         case v if v < 0 => Some(false)
         case _ => None
       }
    }
  }

  /**
    * Returns a satisfying variable assignment if the variable was forced at the toplevel.
    *
    * This function the same as [[PicoSAT.getAssignment(lit)]], but only returns a truth value if the literal is forced
    * to this value at the toplevel. Does not require that [[PicoSAT.solve()]]  was called.
    *
    * @param lit The variable the assignment is for.
    * @return An assignment.
    */
  def getAssignmentToplevel(lit: Int) : Option[Boolean] = {
    lib.picosat_deref_toplevel(context, lit) match {
         case v if v > 0 => Some(true)
         case v if v < 0 => Some(false)
         case _ => None
    }
  }

  /**
    * @return True if the CNF is unsatisfiable because the empty clause was added or derived.
    */
  def inconsistent: Boolean = {
    lib.picosat_inconsistent(context) != 0
  }

  /**
    * Adds a temporary assumption.
    *
    * Adding assumptions is conceptually similar to adding unit clauses containing only the assumed literal. The
    * assumptions however stay only valid for one call to [[PicoSAT.solve()]] and will be removed before the subsequent
    * call to [[PicoSAT.solve()]], expect when assumed again.
    *
    * @param lit The literal to assume.
    */
  def assume(lit : Int): Unit = {
    lib.picosat_assume(context, lit)
  }

  /**
    * Tests if an assumption is a failed assumption.
    *
    * Returns true if the assumption has been used to derive unsatisfiability. Calling this function therefore
    * only makes sense if the state is UNSAT. This is an overapproximation  of the literals necessary to
    * derive unsatisfiability, but as accurate as generating core literals. This function, however, is much
    * more efficient, since tracing is not needed. This should only be called while the assumption is valid.
    * See [[PicoSAT.assume(lit)]] for details.
    *
    * @param lit The assumed literal.
    * @return True if the literal is a failed  assumption.
    */
  def failedAssumption(lit: Int): Boolean = {
    lib.picosat_failed_assumption(context, lit) != 0
  }

  /**
    * Returns an array of failed assumptions. See [[PicoSAT.failedAssumption(lit)]] for details.
    *
    * @return An array of failed assumption.
    */
  def failedAssumptions: List[Int] = {
    lib.picosat_failed_assumptions(context)
  }

  /**
    * Test if the satisfying assumption did not change.
    *
    * Assume that the state was SAT then clauses were added and after calling [[PicoSAT.solve()]] the
    * state was still SAT. Now [[PicoSAT.changed]] returns '''false''' if the assignment to the old
    * variables did not change. The result is only valid until additional clauses or assumptions are
    * added, or [[PicoSAT.solve()]] is called again.
    *
    * @return False if satisfying assignment did not change.
    */
  def changed: Boolean = {
    lib.picosat_changed(context) != 0
  }

  /**
    *  Returns true if the clause with index '''clauseIdx''' is in the clause core. This function needs
    *  tracing to be enabled.
    *
    *  Note: According to the PicoSAT documentation usage of this function has not been tested in
    *  incremental mode with failed assumptions.
    *
    * @param clauseIdx The clause index starting at 0 increasing in the order the clauses have been added.
    * @return True if the clause is in the core.
    */
  def coreClause(clauseIdx: Int): Boolean = {
    if (tracing)
      lib.picosat_coreclause(context, clauseIdx) != 0
    else
      false
  }

  /**
    * Allows to query the variable core. Those are the variables used in the resolution to derive the empty
    * clause if the state is UNSAT. This function needs tracing to be enabled.
    *
    * @param lit The literal in question.
    * @return True if the literal was used to derive the empty clause.
    */
  def coreLiteral(lit: Int): Boolean = {
    if(tracing)
      lib.picosat_corelit(context, lit) != 0
    else
      false
  }

  /**
    * Over approximation of [[PicoSAT.coreLiteral(lit)]]. A literal is a 'used' literal, if it was involved to
    * derive any learned clause. This does not need tracing.
    *
    * @param lit The literal in question.
    * @return True if the literal was used.
    */
  def usedLiteral(lit: Int): Boolean = {
    lib.picosat_usedlit(context, lit) != 0
  }

//  def reset(): Unit = lib.picosat_reset(context)

  def version(): String = {
    lib.picosat_version()
  }

  import com.sun.jna.Library
  protected[this] trait LibPicoSAT extends Library {
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
}

/*
 * Companion object used to create PicoSAT contexts.
 */
object PicoSAT {
  private[this] lazy val globalInstance:PicoSAT = apply(true)

  final def get(): PicoSAT = globalInstance

  /**
   * Returns a new PicoSAT context.
   *
   * @param enableTracing Enables tracing used to genrates cores. Enabling tracing results in the solver using more
   *                      memory.
   */
  final def apply(enableTracing: Boolean = false) : PicoSAT = {
    new PicoSAT(enableTracing) {}
  }

  /**
    * Used to represent the state a context is in.
    */
  sealed abstract class State
  final case object Unknown extends State
  final case object SAT extends State
  final case object UNSAT extends State

  final object State {
    def apply(code: Int): State = code match {
      case 0 => Unknown
      case 10 => SAT
      case 20 => UNSAT
    }
  }
}
