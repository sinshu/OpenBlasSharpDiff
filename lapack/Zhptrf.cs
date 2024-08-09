﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZHPTRF computes the factorization of a complex Hermitian packed
        /// matrix A using the Bunch-Kaufman diagonal pivoting method:
        /// </para>
        /// <para>
        ///    A = U*D*U**H  or  A = L*D*L**H
        /// </para>
        /// <para>
        /// where U (or L) is a product of permutation and unit upper (lower)
        /// triangular matrices, and D is Hermitian and block diagonal with
        /// 1-by-1 and 2-by-2 diagonal blocks.
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
        /// <param name="ap">
        /// [in,out] AP is COMPLEX*16 array, dimension (N*(N+1)/2).
        /// On entry, the upper or lower triangle of the Hermitian matrix
        /// A, packed columnwise in a linear array.  The j-th column of A
        /// is stored in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// 
        /// On exit, the block diagonal matrix D and the multipliers used
        /// to obtain the factor U or L, stored as a packed triangular
        /// matrix overwriting A (see below for further details).
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
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0: if INFO = i, D(i,i) is exactly zero.  The factorization
        /// has been completed, but the block diagonal matrix D is
        /// exactly singular, and division by zero will occur if it
        /// is used to solve a system of equations.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  If UPLO = &#39;U&#39;, then A = U*D*U**H, where
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
        ///  If UPLO = &#39;L&#39;, then A = L*D*L**H, where
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zhptrf", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zhptrf(
            MatrixLayout matrixLayout,
            char uplo,
            int n,
            Complex* ap,
            int* ipiv);
    }
}
