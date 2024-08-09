using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGESVX uses the LU factorization to compute the solution to a complex
        /// system of linear equations
        ///    A * X = B,
        /// where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
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
        /// Specifies whether or not the factored form of the matrix A is
        /// supplied on entry, and if not, whether the matrix A should be
        /// equilibrated before it is factored.
        /// = &#39;F&#39;:  On entry, AF and IPIV contain the factored form of A.
        /// If EQUED is not &#39;N&#39;, the matrix A has been
        /// equilibrated with scaling factors given by R and C.
        /// A, AF, and IPIV are not modified.
        /// = &#39;N&#39;:  The matrix A will be copied to AF and factored.
        /// = &#39;E&#39;:  The matrix A will be equilibrated if necessary, then
        /// copied to AF and factored.
        /// </param>
        /// <param name="trans">
        /// [in] TRANS is CHARACTER*1.
        /// Specifies the form of the system of equations:
        /// = &#39;N&#39;:  A * X = B     (No transpose)
        /// = &#39;T&#39;:  A**T * X = B  (Transpose)
        /// = &#39;C&#39;:  A**H * X = B  (Conjugate transpose)
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of linear equations, i.e., the order of the
        /// matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="nrhs">
        /// [in] NRHS is INTEGER.
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrices B and X.  NRHS &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the N-by-N matrix A.  If FACT = &#39;F&#39; and EQUED is
        /// not &#39;N&#39;, then A must have been equilibrated by the scaling
        /// factors in R and/or C.  A is not modified if FACT = &#39;F&#39; or
        /// &#39;N&#39;, or if FACT = &#39;E&#39; and EQUED = &#39;N&#39; on exit.
        /// 
        /// On exit, if EQUED .ne. &#39;N&#39;, A is scaled as follows:
        /// EQUED = &#39;R&#39;:  A := diag(R) * A
        /// EQUED = &#39;C&#39;:  A := A * diag(C)
        /// EQUED = &#39;B&#39;:  A := diag(R) * A * diag(C).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="af">
        /// [in,out] AF is COMPLEX array, dimension (LDAF,N).
        /// If FACT = &#39;F&#39;, then AF is an input argument and on entry
        /// contains the factors L and U from the factorization
        /// A = P*L*U as computed by CGETRF.  If EQUED .ne. &#39;N&#39;, then
        /// AF is the factored form of the equilibrated matrix A.
        /// 
        /// If FACT = &#39;N&#39;, then AF is an output argument and on exit
        /// returns the factors L and U from the factorization A = P*L*U
        /// of the original matrix A.
        /// 
        /// If FACT = &#39;E&#39;, then AF is an output argument and on exit
        /// returns the factors L and U from the factorization A = P*L*U
        /// of the equilibrated matrix A (see the description of A for
        /// the form of the equilibrated matrix).
        /// </param>
        /// <param name="ldaf">
        /// [in] LDAF is INTEGER.
        /// The leading dimension of the array AF.  LDAF &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [in,out] IPIV is INTEGER array, dimension (N).
        /// If FACT = &#39;F&#39;, then IPIV is an input argument and on entry
        /// contains the pivot indices from the factorization A = P*L*U
        /// as computed by CGETRF; row i of the matrix was interchanged
        /// with row IPIV(i).
        /// 
        /// If FACT = &#39;N&#39;, then IPIV is an output argument and on exit
        /// contains the pivot indices from the factorization A = P*L*U
        /// of the original matrix A.
        /// 
        /// If FACT = &#39;E&#39;, then IPIV is an output argument and on exit
        /// contains the pivot indices from the factorization A = P*L*U
        /// of the equilibrated matrix A.
        /// </param>
        /// <param name="equed">
        /// [in,out] EQUED is CHARACTER*1.
        /// Specifies the form of equilibration that was done.
        /// = &#39;N&#39;:  No equilibration (always true if FACT = &#39;N&#39;).
        /// = &#39;R&#39;:  Row equilibration, i.e., A has been premultiplied by
        /// diag(R).
        /// = &#39;C&#39;:  Column equilibration, i.e., A has been postmultiplied
        /// by diag(C).
        /// = &#39;B&#39;:  Both row and column equilibration, i.e., A has been
        /// replaced by diag(R) * A * diag(C).
        /// EQUED is an input argument if FACT = &#39;F&#39;; otherwise, it is an
        /// output argument.
        /// </param>
        /// <param name="r">
        /// [in,out] R is REAL array, dimension (N).
        /// The row scale factors for A.  If EQUED = &#39;R&#39; or &#39;B&#39;, A is
        /// multiplied on the left by diag(R); if EQUED = &#39;N&#39; or &#39;C&#39;, R
        /// is not accessed.  R is an input argument if FACT = &#39;F&#39;;
        /// otherwise, R is an output argument.  If FACT = &#39;F&#39; and
        /// EQUED = &#39;R&#39; or &#39;B&#39;, each element of R must be positive.
        /// </param>
        /// <param name="c">
        /// [in,out] C is REAL array, dimension (N).
        /// The column scale factors for A.  If EQUED = &#39;C&#39; or &#39;B&#39;, A is
        /// multiplied on the right by diag(C); if EQUED = &#39;N&#39; or &#39;R&#39;, C
        /// is not accessed.  C is an input argument if FACT = &#39;F&#39;;
        /// otherwise, C is an output argument.  If FACT = &#39;F&#39; and
        /// EQUED = &#39;C&#39; or &#39;B&#39;, each element of C must be positive.
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX array, dimension (LDB,NRHS).
        /// On entry, the N-by-NRHS right hand side matrix B.
        /// On exit,
        /// if EQUED = &#39;N&#39;, B is not modified;
        /// if TRANS = &#39;N&#39; and EQUED = &#39;R&#39; or &#39;B&#39;, B is overwritten by
        /// diag(R)*B;
        /// if TRANS = &#39;T&#39; or &#39;C&#39; and EQUED = &#39;C&#39; or &#39;B&#39;, B is
        /// overwritten by diag(C)*B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [out] X is COMPLEX array, dimension (LDX,NRHS).
        /// If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X
        /// to the original system of equations.  Note that A and B are
        /// modified on exit if EQUED .ne. &#39;N&#39;, and the solution to the
        /// equilibrated system is inv(diag(C))*X if TRANS = &#39;N&#39; and
        /// EQUED = &#39;C&#39; or &#39;B&#39;, or inv(diag(R))*X if TRANS = &#39;T&#39; or &#39;C&#39;
        /// and EQUED = &#39;R&#39; or &#39;B&#39;.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is REAL.
        /// The estimate of the reciprocal condition number of the matrix
        /// A after equilibration (if done).  If RCOND is less than the
        /// machine precision (in particular, if RCOND = 0), the matrix
        /// is singular to working precision.  This condition is
        /// indicated by a return code of INFO &gt; 0.
        /// </param>
        /// <param name="ferr">
        /// [out] FERR is REAL array, dimension (NRHS).
        /// The estimated forward error bound for each solution vector
        /// X(j) (the j-th column of the solution matrix X).
        /// If XTRUE is the true solution corresponding to X(j), FERR(j)
        /// is an estimated upper bound for the magnitude of the largest
        /// element in (X(j) - XTRUE) divided by the magnitude of the
        /// largest element in X(j).  The estimate is as reliable as
        /// the estimate for RCOND, and is almost always a slight
        /// overestimate of the true error.
        /// </param>
        /// <param name="berr">
        /// [out] BERR is REAL array, dimension (NRHS).
        /// The componentwise relative backward error of each solution
        /// vector X(j) (i.e., the smallest relative change in
        /// any element of A or B that makes X(j) an exact solution).
        /// </param>
        /// <param name="rpivot">
        /// No description available.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, and i is
        /// &lt;= N:  U(i,i) is exactly zero.  The factorization has
        /// been completed, but the factor U is exactly
        /// singular, so the solution and error bounds
        /// could not be computed. RCOND = 0 is returned.
        /// = N+1: U is nonsingular, but RCOND is less than machine
        /// precision, meaning that the matrix is singular
        /// to working precision.  Nevertheless, the
        /// solution and error bounds are computed because
        /// there are a number of situations where the
        /// computed solution can be more accurate than the
        /// value of RCOND would suggest.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgesvx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgesvx(
            MatrixLayout matrixLayout,
            char fact,
            char trans,
            int n,
            int nrhs,
            Complex32* a,
            int lda,
            Complex32* af,
            int ldaf,
            int* ipiv,
            ref char equed,
            float* r,
            float* c,
            Complex32* b,
            int ldb,
            Complex32* x,
            int ldx,
            float* rcond,
            float* ferr,
            float* berr,
            float* rpivot);
    }
}
