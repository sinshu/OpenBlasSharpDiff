using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SPTSVX uses the factorization A = L*D*L**T to compute the solution
        /// to a real system of linear equations A*X = B, where A is an N-by-N
        /// symmetric positive definite tridiagonal matrix and X and B are
        /// N-by-NRHS matrices.
        /// </para>
        /// <para>
        /// Error bounds on the solution and a condition estimate are also
        /// provided.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="fact">
        /// [in] FACT is CHARACTER*1.
        /// Specifies whether or not the factored form of A has been
        /// supplied on entry.
        /// = &#39;F&#39;:  On entry, DF and EF contain the factored form of A.
        /// D, E, DF, and EF will not be modified.
        /// = &#39;N&#39;:  The matrix A will be copied to DF and EF and
        /// factored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrices B and X.  NRHS &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in] D is REAL array, dimension (N).
        /// The n diagonal elements of the tridiagonal matrix A.
        /// </param>
        /// <param name="e">
        /// [in] E is REAL array, dimension (N-1).
        /// The (n-1) subdiagonal elements of the tridiagonal matrix A.
        /// </param>
        /// <param name="df">
        /// [in,out] DF is REAL array, dimension (N).
        /// If FACT = &#39;F&#39;, then DF is an input argument and on entry
        /// contains the n diagonal elements of the diagonal matrix D
        /// from the L*D*L**T factorization of A.
        /// If FACT = &#39;N&#39;, then DF is an output argument and on exit
        /// contains the n diagonal elements of the diagonal matrix D
        /// from the L*D*L**T factorization of A.
        /// </param>
        /// <param name="ef">
        /// [in,out] EF is REAL array, dimension (N-1).
        /// If FACT = &#39;F&#39;, then EF is an input argument and on entry
        /// contains the (n-1) subdiagonal elements of the unit
        /// bidiagonal factor L from the L*D*L**T factorization of A.
        /// If FACT = &#39;N&#39;, then EF is an output argument and on exit
        /// contains the (n-1) subdiagonal elements of the unit
        /// bidiagonal factor L from the L*D*L**T factorization of A.
        /// </param>
        /// <param name="b">
        /// [in] B is REAL array, dimension (LDB,NRHS).
        /// The N-by-NRHS right hand side matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [out] X is REAL array, dimension (LDX,NRHS).
        /// If INFO = 0 of INFO = N+1, the N-by-NRHS solution matrix X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is REAL.
        /// The reciprocal condition number of the matrix A.  If RCOND
        /// is less than the machine precision (in particular, if
        /// RCOND = 0), the matrix is singular to working precision.
        /// This condition is indicated by a return code of INFO &gt; 0.
        /// </param>
        /// <param name="ferr">
        /// [out] FERR is REAL array, dimension (NRHS).
        /// The forward error bound for each solution vector
        /// X(j) (the j-th column of the solution matrix X).
        /// If XTRUE is the true solution corresponding to X(j), FERR(j)
        /// is an estimated upper bound for the magnitude of the largest
        /// element in (X(j) - XTRUE) divided by the magnitude of the
        /// largest element in X(j).
        /// </param>
        /// <param name="berr">
        /// [out] BERR is REAL array, dimension (NRHS).
        /// The componentwise relative backward error of each solution
        /// vector X(j) (i.e., the smallest relative change in any
        /// element of A or B that makes X(j) an exact solution).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, and i is
        /// &lt;= N:  the leading principal minor of order i of A
        /// is not positive, so the factorization could not
        /// be completed, and the solution has not been
        /// computed. RCOND = 0 is returned.
        /// = N+1: U is nonsingular, but RCOND is less than machine
        /// precision, meaning that the matrix is singular
        /// to working precision.  Nevertheless, the
        /// solution and error bounds are computed because
        /// there are a number of situations where the
        /// computed solution can be more accurate than the
        /// value of RCOND would suggest.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sptsvx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sptsvx(
            MatrixLayout matrixLayout,
            char fact,
            int n,
            int nrhs,
            float* d,
            float* e,
            float* df,
            float* ef,
            float* b,
            int ldb,
            float* x,
            int ldx,
            float* rcond,
            float* ferr,
            float* berr);
    }
}
