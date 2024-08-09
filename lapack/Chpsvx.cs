using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CHPSVX uses the diagonal pivoting factorization A = U*D*U**H or
        /// A = L*D*L**H to compute the solution to a complex system of linear
        /// equations A * X = B, where A is an N-by-N Hermitian matrix stored
        /// in packed format and X and B are N-by-NRHS matrices.
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
        /// = &#39;F&#39;:  On entry, AFP and IPIV contain the factored form of
        /// A.  AFP and IPIV will not be modified.
        /// = &#39;N&#39;:  The matrix A will be copied to AFP and factored.
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
        /// <param name="ap">
        /// [in] AP is COMPLEX array, dimension (N*(N+1)/2).
        /// The upper or lower triangle of the Hermitian matrix A, packed
        /// columnwise in a linear array.  The j-th column of A is stored
        /// in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// See below for further details.
        /// </param>
        /// <param name="afp">
        /// [in,out] AFP is COMPLEX array, dimension (N*(N+1)/2).
        /// If FACT = &#39;F&#39;, then AFP is an input argument and on entry
        /// contains the block diagonal matrix D and the multipliers used
        /// to obtain the factor U or L from the factorization
        /// A = U*D*U**H or A = L*D*L**H as computed by CHPTRF, stored as
        /// a packed triangular matrix in the same storage format as A.
        /// 
        /// If FACT = &#39;N&#39;, then AFP is an output argument and on exit
        /// contains the block diagonal matrix D and the multipliers used
        /// to obtain the factor U or L from the factorization
        /// A = U*D*U**H or A = L*D*L**H as computed by CHPTRF, stored as
        /// a packed triangular matrix in the same storage format as A.
        /// </param>
        /// <param name="ipiv">
        /// [in,out] IPIV is INTEGER array, dimension (N).
        /// If FACT = &#39;F&#39;, then IPIV is an input argument and on entry
        /// contains details of the interchanges and the block structure
        /// of D, as determined by CHPTRF.
        /// If IPIV(k) &gt; 0, then rows and columns k and IPIV(k) were
        /// interchanged and D(k,k) is a 1-by-1 diagonal block.
        /// If UPLO = &#39;U&#39; and IPIV(k) = IPIV(k-1) &lt; 0, then rows and
        /// columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k)
        /// is a 2-by-2 diagonal block.  If UPLO = &#39;L&#39; and IPIV(k) =
        /// IPIV(k+1) &lt; 0, then rows and columns k+1 and -IPIV(k) were
        /// interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
        /// 
        /// If FACT = &#39;N&#39;, then IPIV is an output argument and on exit
        /// contains details of the interchanges and the block structure
        /// of D, as determined by CHPTRF.
        /// </param>
        /// <param name="b">
        /// [in] B is COMPLEX array, dimension (LDB,NRHS).
        /// The N-by-NRHS right hand side matrix B.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="x">
        /// [out] X is COMPLEX array, dimension (LDX,NRHS).
        /// If INFO = 0 or INFO = N+1, the N-by-NRHS solution matrix X.
        /// </param>
        /// <param name="ldx">
        /// [in] LDX is INTEGER.
        /// The leading dimension of the array X.  LDX &gt;= max(1,N).
        /// </param>
        /// <param name="rcond">
        /// [out] RCOND is REAL.
        /// The estimate of the reciprocal condition number of the matrix
        /// A.  If RCOND is less than the machine precision (in
        /// particular, if RCOND = 0), the matrix is singular to working
        /// precision.  This condition is indicated by a return code of
        /// INFO &gt; 0.
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
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, and i is
        /// &lt;= N:  D(i,i) is exactly zero.  The factorization
        /// has been completed but the factor D is exactly
        /// singular, so the solution and error bounds could
        /// not be computed. RCOND = 0 is returned.
        /// = N+1: D is nonsingular, but RCOND is less than machine
        /// precision, meaning that the matrix is singular
        /// to working precision.  Nevertheless, the
        /// solution and error bounds are computed because
        /// there are a number of situations where the
        /// computed solution can be more accurate than the
        /// value of RCOND would suggest.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The packed storage scheme is illustrated by the following example
        ///  when N = 4, UPLO = &#39;U&#39;:
        /// </para>
        /// <para>
        ///  Two-dimensional storage of the Hermitian matrix A:
        /// </para>
        /// <para>
        ///     a11 a12 a13 a14
        ///         a22 a23 a24
        ///             a33 a34     (aij = conjg(aji))
        ///                 a44
        /// </para>
        /// <para>
        ///  Packed storage of the upper triangle of A:
        /// </para>
        /// <para>
        ///  AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_chpsvx", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Chpsvx(
            MatrixLayout matrixLayout,
            char fact,
            char uplo,
            int n,
            int nrhs,
            Complex32* ap,
            Complex32* afp,
            int* ipiv,
            Complex32* b,
            int ldb,
            Complex32* x,
            int ldx,
            float* rcond,
            float* ferr,
            float* berr);
    }
}
