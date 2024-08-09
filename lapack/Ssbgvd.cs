using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SSBGVD computes all the eigenvalues, and optionally, the eigenvectors
        /// of a real generalized symmetric-definite banded eigenproblem, of the
        /// form A*x=(lambda)*B*x.  Here A and B are assumed to be symmetric and
        /// banded, and B is also positive definite.  If eigenvectors are
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
        /// or the number of subdiagonals if UPLO = &#39;L&#39;.  KA &gt;= 0.
        /// </param>
        /// <param name="kb">
        /// [in] KB is INTEGER.
        /// The number of superdiagonals of the matrix B if UPLO = &#39;U&#39;,
        /// or the number of subdiagonals if UPLO = &#39;L&#39;.  KB &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in,out] AB is REAL array, dimension (LDAB, N).
        /// On entry, the upper or lower triangle of the symmetric band
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
        /// [in,out] BB is REAL array, dimension (LDBB, N).
        /// On entry, the upper or lower triangle of the symmetric band
        /// matrix B, stored in the first kb+1 rows of the array.  The
        /// j-th column of B is stored in the j-th column of the array BB
        /// as follows:
        /// if UPLO = &#39;U&#39;, BB(ka+1+i-j,j) = B(i,j) for max(1,j-kb)&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, BB(1+i-j,j)    = B(i,j) for j&lt;=i&lt;=min(n,j+kb).
        /// 
        /// On exit, the factor S from the split Cholesky factorization
        /// B = S**T*S, as returned by SPBSTF.
        /// </param>
        /// <param name="ldbb">
        /// [in] LDBB is INTEGER.
        /// The leading dimension of the array BB.  LDBB &gt;= KB+1.
        /// </param>
        /// <param name="w">
        /// [out] W is REAL array, dimension (N).
        /// If INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is REAL array, dimension (LDZ, N).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, Z contains the matrix Z of
        /// eigenvectors, with the i-th column of Z holding the
        /// eigenvector associated with W(i).  The eigenvectors are
        /// normalized so Z**T*B*Z = I.
        /// If JOBZ = &#39;N&#39;, then Z is not referenced.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// JOBZ = &#39;V&#39;, LDZ &gt;= max(1,N).
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, and i is:
        /// &lt;= N:  the algorithm failed to converge:
        /// i off-diagonal elements of an intermediate
        /// tridiagonal form did not converge to zero;
        /// &gt; N:   if INFO = N + i, for 1 &lt;= i &lt;= N, then SPBSTF
        /// returned INFO = i: B is not positive definite.
        /// The factorization of B could not be completed and
        /// no eigenvalues or eigenvectors were computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ssbgvd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ssbgvd(
            MatrixLayout matrixLayout,
            char jobz,
            char uplo,
            int n,
            int ka,
            int kb,
            float* ab,
            int ldab,
            float* bb,
            int ldbb,
            float* w,
            float* z,
            int ldz);
    }
}
