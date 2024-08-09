using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZHBGVD computes all the eigenvalues, and optionally, the eigenvectors
        /// of a complex generalized Hermitian-definite banded eigenproblem, of
        /// the form A*x=(lambda)*B*x. Here A and B are assumed to be Hermitian
        /// and banded, and B is also positive definite.  If eigenvectors are
        /// desired, it uses a divide and conquer algorithm.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobz">
        /// [in] JOBZ is CHARACTER*1.
        /// = &#39;N&#39;:  Compute eigenvalues only;
        /// = &#39;V&#39;:  Compute eigenvalues and eigenvectors.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  Upper triangles of A and B are stored;
        /// = &#39;L&#39;:  Lower triangles of A and B are stored.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrices A and B.  N &gt;= 0.
        /// </param>
        /// <param name="ka">
        /// [in] KA is INTEGER.
        /// The number of superdiagonals of the matrix A if UPLO = &#39;U&#39;,
        /// or the number of subdiagonals if UPLO = &#39;L&#39;. KA &gt;= 0.
        /// </param>
        /// <param name="kb">
        /// [in] KB is INTEGER.
        /// The number of superdiagonals of the matrix B if UPLO = &#39;U&#39;,
        /// or the number of subdiagonals if UPLO = &#39;L&#39;. KB &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in,out] AB is COMPLEX*16 array, dimension (LDAB, N).
        /// On entry, the upper or lower triangle of the Hermitian band
        /// matrix A, stored in the first ka+1 rows of the array.  The
        /// j-th column of A is stored in the j-th column of the array AB
        /// as follows:
        /// if UPLO = &#39;U&#39;, AB(ka+1+i-j,j) = A(i,j) for max(1,j-ka)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AB(1+i-j,j)    = A(i,j) for j&lt;=i&lt;=min(n,j+ka).
        /// 
        /// On exit, the contents of AB are destroyed.
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array AB.  LDAB &gt;= KA+1.
        /// </param>
        /// <param name="bb">
        /// [in,out] BB is COMPLEX*16 array, dimension (LDBB, N).
        /// On entry, the upper or lower triangle of the Hermitian band
        /// matrix B, stored in the first kb+1 rows of the array.  The
        /// j-th column of B is stored in the j-th column of the array BB
        /// as follows:
        /// if UPLO = &#39;U&#39;, BB(kb+1+i-j,j) = B(i,j) for max(1,j-kb)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, BB(1+i-j,j)    = B(i,j) for j&lt;=i&lt;=min(n,j+kb).
        /// 
        /// On exit, the factor S from the split Cholesky factorization
        /// B = S**H*S, as returned by ZPBSTF.
        /// </param>
        /// <param name="ldbb">
        /// [in] LDBB is INTEGER.
        /// The leading dimension of the array BB.  LDBB &gt;= KB+1.
        /// </param>
        /// <param name="w">
        /// [out] W is DOUBLE PRECISION array, dimension (N).
        /// If INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is COMPLEX*16 array, dimension (LDZ, N).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, Z contains the matrix Z of
        /// eigenvectors, with the i-th column of Z holding the
        /// eigenvector associated with W(i). The eigenvectors are
        /// normalized so that Z**H*B*Z = I.
        /// If JOBZ = &#39;N&#39;, then Z is not referenced.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// JOBZ = &#39;V&#39;, LDZ &gt;= N.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, and i is:
        /// &lt;= N:  the algorithm failed to converge:
        /// i off-diagonal elements of an intermediate
        /// tridiagonal form did not converge to zero;
        /// &gt; N:   if INFO = N + i, for 1 &lt;= i &lt;= N, then ZPBSTF
        /// returned INFO = i: B is not positive definite.
        /// The factorization of B could not be completed and
        /// no eigenvalues or eigenvectors were computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zhbgvd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zhbgvd(
            MatrixLayout matrixLayout,
            char jobz,
            char uplo,
            int n,
            int ka,
            int kb,
            Complex* ab,
            int ldab,
            Complex* bb,
            int ldbb,
            double* w,
            Complex* z,
            int ldz);
    }
}
