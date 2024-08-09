using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CPPSV computes the solution to a complex system of linear equations
        ///    A * X = B,
        /// where A is an N-by-N Hermitian positive definite matrix stored in
        /// packed format and X and B are N-by-NRHS matrices.
        /// </para>
        /// <para>
        /// The Cholesky decomposition is used to factor A as
        ///    A = U**H * U,  if UPLO = &#39;U&#39;, or
        ///    A = L * L**H,  if UPLO = &#39;L&#39;,
        /// where U is an upper triangular matrix and L is a lower triangular
        /// matrix.  The factored form of A is then used to solve the system of
        /// equations A * X = B.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
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
        /// of the matrix B.  NRHS &gt;= 0.
        /// </param>
        /// <param name="ap">
        /// [in,out] AP is COMPLEX array, dimension (N*(N+1)/2).
        /// On entry, the upper or lower triangle of the Hermitian matrix
        /// A, packed columnwise in a linear array.  The j-th column of A
        /// is stored in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// See below for further details.
        /// 
        /// On exit, if INFO = 0, the factor U or L from the Cholesky
        /// factorization A = U**H*U or A = L*L**H, in the same storage
        /// format as A.
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX array, dimension (LDB,NRHS).
        /// On entry, the N-by-NRHS right hand side matrix B.
        /// On exit, if INFO = 0, the N-by-NRHS solution matrix X.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, the leading principal minor of order i
        /// of A is not positive, so the factorization could not
        /// be completed, and the solution has not been computed.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cppsv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cppsv(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            Complex32* ap,
            Complex32* b,
            int ldb);
    }
}
