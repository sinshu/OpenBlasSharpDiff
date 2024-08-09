using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CSYTRF computes the factorization of a complex symmetric matrix A
        /// using the Bunch-Kaufman diagonal pivoting method.  The form of the
        /// factorization is
        /// </para>
        /// <para>
        ///    A = U*D*U**T  or  A = L*D*L**T
        /// </para>
        /// <para>
        /// where U (or L) is a product of permutation and unit upper (lower)
        /// triangular matrices, and D is symmetric and block diagonal with
        /// 1-by-1 and 2-by-2 diagonal blocks.
        /// </para>
        /// <para>
        /// This is the blocked version of the algorithm, calling Level 3 BLAS.
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
        /// The order of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the leading
        /// N-by-N upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = &#39;L&#39;, the
        /// leading N-by-N lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// 
        /// On exit, the block diagonal matrix D and the multipliers used
        /// to obtain the factor U or L (see below for further details).
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="ipiv">
        /// [out] IPIV is INTEGER array, dimension (N).
        /// Details of the interchanges and the block structure of D.
        /// If IPIV(k) &gt; 0, then rows and columns k and IPIV(k) were
        /// interchanged and D(k,k) is a 1-by-1 diagonal block.
        /// If UPLO = &#39;U&#39; and IPIV(k) = IPIV(k-1) &lt; 0, then rows and
        /// columns k-1 and -IPIV(k) were interchanged and D(k-1:k,k-1:k)
        /// is a 2-by-2 diagonal block.  If UPLO = &#39;L&#39; and IPIV(k) =
        /// IPIV(k+1) &lt; 0, then rows and columns k+1 and -IPIV(k) were
        /// interchanged and D(k:k+1,k:k+1) is a 2-by-2 diagonal block.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, D(i,i) is exactly zero.  The factorization
        /// has been completed, but the block diagonal matrix D is
        /// exactly singular, and division by zero will occur if it
        /// is used to solve a system of equations.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  If UPLO = &#39;U&#39;, then A = U*D*U**T, where
        ///     U = P(n)*U(n)* ... *P(k)U(k)* ...,
        ///  i.e., U is a product of terms P(k)*U(k), where k decreases from n to
        ///  1 in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1
        ///  and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as
        ///  defined by IPIV(k), and U(k) is a unit upper triangular matrix, such
        ///  that if the diagonal block D(k) is of order s (s = 1 or 2), then
        /// </para>
        /// <para>
        ///             (   I    v    0   )   k-s
        ///     U(k) =  (   0    I    0   )   s
        ///             (   0    0    I   )   n-k
        ///                k-s   s   n-k
        /// </para>
        /// <para>
        ///  If s = 1, D(k) overwrites A(k,k), and v overwrites A(1:k-1,k).
        ///  If s = 2, the upper triangle of D(k) overwrites A(k-1,k-1), A(k-1,k),
        ///  and A(k,k), and v overwrites A(1:k-2,k-1:k).
        /// </para>
        /// <para>
        ///  If UPLO = &#39;L&#39;, then A = L*D*L**T, where
        ///     L = P(1)*L(1)* ... *P(k)*L(k)* ...,
        ///  i.e., L is a product of terms P(k)*L(k), where k increases from 1 to
        ///  n in steps of 1 or 2, and D is a block diagonal matrix with 1-by-1
        ///  and 2-by-2 diagonal blocks D(k).  P(k) is a permutation matrix as
        ///  defined by IPIV(k), and L(k) is a unit lower triangular matrix, such
        ///  that if the diagonal block D(k) is of order s (s = 1 or 2), then
        /// </para>
        /// <para>
        ///             (   I    0     0   )  k-1
        ///     L(k) =  (   0    I     0   )  s
        ///             (   0    v     I   )  n-k-s+1
        ///                k-1   s  n-k-s+1
        /// </para>
        /// <para>
        ///  If s = 1, D(k) overwrites A(k,k), and v overwrites A(k+1:n,k).
        ///  If s = 2, the lower triangle of D(k) overwrites A(k,k), A(k+1,k),
        ///  and A(k+1,k+1), and v overwrites A(k+2:n,k:k+1).
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_csytrf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Csytrf(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex32* a,
            int lda,
            int* ipiv);
    }
}
