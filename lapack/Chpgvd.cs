using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CHPGVD computes all the eigenvalues and, optionally, the eigenvectors
        /// of a complex generalized Hermitian-definite eigenproblem, of the form
        /// A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
        /// B are assumed to be Hermitian, stored in packed format, and B is also
        /// positive definite.
        /// If eigenvectors are desired, it uses a divide and conquer algorithm.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="itype">
        /// [in] ITYPE is INTEGER.
        /// Specifies the problem type to be solved:
        /// = 1:  A*x = (lambda)*B*x
        /// = 2:  A*B*x = (lambda)*x
        /// = 3:  B*A*x = (lambda)*x
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
        /// <param name="ap">
        /// [in,out] AP is COMPLEX array, dimension (N*(N+1)/2).
        /// On entry, the upper or lower triangle of the Hermitian matrix
        /// A, packed columnwise in a linear array.  The j-th column of A
        /// is stored in the array AP as follows:
        /// if UPLO = &#39;U&#39;, AP(i + (j-1)*j/2) = A(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, AP(i + (j-1)*(2*n-j)/2) = A(i,j) for j&lt;=i&lt;=n.
        /// 
        /// On exit, the contents of AP are destroyed.
        /// </param>
        /// <param name="bp">
        /// [in,out] BP is COMPLEX array, dimension (N*(N+1)/2).
        /// On entry, the upper or lower triangle of the Hermitian matrix
        /// B, packed columnwise in a linear array.  The j-th column of B
        /// is stored in the array BP as follows:
        /// if UPLO = &#39;U&#39;, BP(i + (j-1)*j/2) = B(i,j) for 1&lt;=i&lt;=j;
        /// if UPLO = &#39;L&#39;, BP(i + (j-1)*(2*n-j)/2) = B(i,j) for j&lt;=i&lt;=n.
        /// 
        /// On exit, the triangular factor U or L from the Cholesky
        /// factorization B = U**H*U or B = L*L**H, in the same storage
        /// format as B.
        /// </param>
        /// <param name="w">
        /// [out] W is REAL array, dimension (N).
        /// If INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is COMPLEX array, dimension (LDZ, N).
        /// If JOBZ = &#39;V&#39;, then if INFO = 0, Z contains the matrix Z of
        /// eigenvectors.  The eigenvectors are normalized as follows:
        /// if ITYPE = 1 or 2, Z**H*B*Z = I;
        /// if ITYPE = 3, Z**H*inv(B)*Z = I.
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
        /// &gt; 0:  CPPTRF or CHPEVD returned an error code:
        /// &lt;= N:  if INFO = i, CHPEVD failed to converge;
        /// i off-diagonal elements of an intermediate
        /// tridiagonal form did not convergeto zero;
        /// &gt; N:   if INFO = N + i, for 1 &lt;= i &lt;= n, then the leading
        /// principal minor of order i of B is not positive.
        /// The factorization of B could not be completed and
        /// no eigenvalues or eigenvectors were computed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_chpgvd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Chpgvd(
            MatrixLayout matrixLayout,
            int itype,
            char jobz,
            char uplo,
            int n,
            Complex32* ap,
            Complex32* bp,
            float* w,
            Complex32* z,
            int ldz);
    }
}
