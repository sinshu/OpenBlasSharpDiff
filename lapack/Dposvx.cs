using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DPOSVX uses the Cholesky factorization A = U**T*U or A = L*L**T to
        /// compute the solution to a real system of linear equations
        ///    A * X = B,
        /// where A is an N-by-N symmetric positive definite matrix and X and B
        /// are N-by-NRHS matrices.
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
        /// = &#39;F&#39;:  On entry, AF contains the factored form of A.
        /// If EQUED = &#39;Y&#39;, the matrix A has been equilibrated
        /// with scaling factors given by S.  A and AF will not
        /// be modified.
        /// = &#39;N&#39;:  The matrix A will be copied to AF and factored.
        /// = &#39;E&#39;:  The matrix A will be equilibrated if necessary, then
        /// copied to AF and factored.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangle of A is stored;
        /// = &#39;L&#39;:  Lower triangle of A is stored.
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
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the symmetric matrix A, except if FACT = &#39;F&#39; and
        /// EQUED = &#39;Y&#39;, then A must contain the equilibrated matrix
        /// diag(S)*A*diag(S).  If UPLO = &#39;U&#39;, the leading
        /// N-by-N upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.  A is not modified if
        /// FACT = &#39;F&#39; or &#39;N&#39;, or if FACT = &#39;E&#39; and EQUED = &#39;N&#39; on exit.
        /// 
        /// On exit, if FACT = &#39;E&#39; and EQUED = &#39;Y&#39;, A is overwritten by
        /// diag(S)*A*diag(S).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="af">
        /// [in,out] AF is DOUBLE PRECISION array, dimension (LDAF,N).
        /// If FACT = &#39;F&#39;, then AF is an input argument and on entry
        /// contains the triangular factor U or L from the Cholesky
        /// factorization A = U**T*U or A = L*L**T, in the same storage
        /// format as A.  If EQUED .ne. &#39;N&#39;, then AF is the factored form
        /// of the equilibrated matrix diag(S)*A*diag(S).
        /// 
        /// If FACT = &#39;N&#39;, then AF is an output argument and on exit
        /// returns the triangular factor U or L from the Cholesky
        /// factorization A = U**T*U or A = L*L**T of the original
        /// matrix A.
        /// 
        /// If FACT = &#39;E&#39;, then AF is an output argument and on exit
        /// returns the triangular factor U or L from the Cholesky
        /// factorization A = U**T*U or A = L*L**T of the equilibrated
        /// matrix A (see the description of A for the form of the
        /// equilibrated matrix).
        /// </param>
        /// <param name="ldaf">
        /// [in] LDAF is INTEGER.
        /// The leading dimension of the array AF.  LDAF &gt;= max(1,N).
        /// </param>
        /// <param name="equed">
        /// [in,out] EQUED is CHARACTER*1.
        /// Specifies the form of equilibration that was done.
        /// = &#39;N&#39;:  No equilibration (always true if FACT = &#39;N&#39;).
        /// = &#39;Y&#39;:  Equilibration was done, i.e., A has been replaced by
        /// diag(S) * A * diag(S).
        /// EQUED is an input argument if FACT = &#39;F&#39;; otherwise, it is an
        /// output argument.
        /// </param>
        /// <param name="s">
        /// [in,out] S is DOUBLE PRECISION array, dimension (N).
        /// The scale factors for A; not accessed if EQUED = &#39;N&#39;.  S is
        /// an input argument if FACT = &#39;F&#39;; otherwise, S is an output
        /// argument.  If FACT = &#39;F&#39; and EQUED = &#39;Y&#39;, each element of S
        /// must be positive.
        /// </param>
        /// <param name="b">
        /// [in,out] B is DOUBLE PRECISION array, dimension (LDB,NRHS).
        /// On entry, the N-by-NRHS right hand side matrix B.
        /// On exit, if EQUED = &#39;N&#39;, B is not modified; if EQUED = &#39;Y&#39;,
        /// B is overwritten by diag(S) * B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [out] X is DOUBLE PRECISION array, dimension (LDX,NRHS).
        /// If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X to
        /// the original system of equations.  Note that if EQUED = &#39;Y&#39;,
        /// A and B are modified on exit, and the solution to the
        /// equilibrated system is inv(diag(S))*X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is DOUBLE PRECISION.
        /// The estimate of the reciprocal condition number of the matrix
        /// A after equilibration (if done).  If RCOND is less than the
        /// machine precision (in particular, if RCOND = 0), the matrix
        /// is singular to working precision.  This condition is
        /// indicated by a return code of INFO &gt; 0.
        /// </param>
        /// <param name="ferr">
        /// [out] FERR is DOUBLE PRECISION array, dimension (NRHS).
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
        /// [out] BERR is DOUBLE PRECISION array, dimension (NRHS).
        /// The componentwise relative backward error of each solution
        /// vector X(j) (i.e., the smallest relative change in
        /// any element of A or B that makes X(j) an exact solution).
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, and i is
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dposvx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dposvx(
            MatrixLayout matrixLayout,
            char fact,
            char uplo,
            int n,
            int nrhs,
            double* a,
            int lda,
            double* af,
            int ldaf,
            ref char equed,
            double* s,
            double* b,
            int ldb,
            double* x,
            int ldx,
            double* rcond,
            double* ferr,
            double* berr);
    }
}
