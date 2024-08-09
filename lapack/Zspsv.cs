using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZSPSV computes the solution to a complex system of linear equations
        ///    A * X = B,
        /// where A is an N-by-N symmetric matrix stored in packed format and X
        /// and B are N-by-NRHS matrices.
        /// </para>
        /// <para>
        /// The diagonal pivoting method is used to factor A as
        ///    A = U * D * U**T,  if UPLO = &#39;U&#39;, or
        ///    A = L * D * L**T,  if UPLO = &#39;L&#39;,
        /// where U (or L) is a product of permutation and unit upper (lower)
        /// triangular matrices, D is symmetric and block diagonal with 1-by-1
        /// and 2-by-2 diagonal blocks.  The factored form of A is then used to
        /// solve the system of equations A * X = B.
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
        /// [in,out] AP is COMPLEX*16 array, dimension (N*(N+1)/2).
        /// On entry, the upper or lower triangle of the symmetric matrix
        /// A, packed columnwise in a linear array.  The j-th column of A
        /// is stored in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// See below for further details.
        /// 
        /// On exit, the block diagonal matrix D and the multipliers used
        /// to obtain the factor U or L from the factorization
        /// A = U*D*U**T or A = L*D*L**T as computed by ZSPTRF, stored as
        /// a packed triangular matrix in the same storage format as A.
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the block structure of D, as
        /// determined by ZSPTRF.  If IPIV(k) &gt; 0, then rows and columns
        /// k and IPIV(k) were interchanged, and D(k,k) is a 1-by-1
        /// diagonal block.  If UPLO = &#39;U&#39; and IPIV(k) = IPIV(k-1) &lt; 0,
        /// then rows and columns k-1 and -IPIV(k) were interchanged and
        /// D(k-1:k,k-1:k) is a 2-by-2 diagonal block.  If UPLO = &#39;L&#39; and
        /// IPIV(k) = IPIV(k+1) &lt; 0, then rows and columns k+1 and
        /// -IPIV(k) were interchanged and D(k:k+1,k:k+1) is a 2-by-2
        /// diagonal block.
        /// </param>
        /// <param name="b">
        /// [in,out] B is COMPLEX*16 array, dimension (LDB,NRHS).
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
        /// &gt; 0:  if INFO = i, D(i,i) is exactly zero.  The factorization
        /// has been completed, but the block diagonal matrix D is
        /// exactly singular, so the solution could not be
        /// computed.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The packed storage scheme is illustrated by the following example
        ///  when N = 4, UPLO = &#39;U&#39;:
        /// </para>
        /// <para>
        ///  Two-dimensional storage of the symmetric matrix A:
        /// </para>
        /// <para>
        ///     a11 a12 a13 a14
        ///         a22 a23 a24
        ///             a33 a34     (aij = aji)
        ///                 a44
        /// </para>
        /// <para>
        ///  Packed storage of the upper triangle of A:
        /// </para>
        /// <para>
        ///  AP = [ a11, a12, a22, a13, a23, a33, a14, a24, a34, a44 ]
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zspsv", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zspsv(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            int nrhs,
            Complex* ap,
            int* ipiv,
            Complex* b,
            int ldb);
    }
}
