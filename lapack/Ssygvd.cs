﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SSYGVD computes all the eigenvalues, and optionally, the eigenvectors
        /// of a real generalized symmetric-definite eigenproblem, of the form
        /// A*x=(lambda)*B*x,  A*Bx=(lambda)*x,  or B*A*x=(lambda)*x.  Here A and
        /// B are assumed to be symmetric and B is also positive definite.
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
        /// <param name="a">
        /// [in,out] A is REAL array, dimension (LDA, N).
        /// On entry, the symmetric matrix A.  If UPLO = &#39;U&#39;, the
        /// leading N-by-N upper triangular part of A contains the
        /// upper triangular part of the matrix A.  If UPLO = &#39;L&#39;,
        /// the leading N-by-N lower triangular part of A contains
        /// the lower triangular part of the matrix A.
        /// 
        /// On exit, if JOBZ = &#39;V&#39;, then if INFO = 0, A contains the
        /// matrix Z of eigenvectors.  The eigenvectors are normalized
        /// as follows:
        /// if ITYPE = 1 or 2, Z**T*B*Z = I;
        /// if ITYPE = 3, Z**T*inv(B)*Z = I.
        /// If JOBZ = &#39;N&#39;, then on exit the upper triangle (if UPLO=&#39;U&#39;)
        /// or the lower triangle (if UPLO=&#39;L&#39;) of A, including the
        /// diagonal, is destroyed.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,N).
        /// </param>
        /// <param name="b">
        /// [in,out] B is REAL array, dimension (LDB, N).
        /// On entry, the symmetric matrix B.  If UPLO = &#39;U&#39;, the
        /// leading N-by-N upper triangular part of B contains the
        /// upper triangular part of the matrix B.  If UPLO = &#39;L&#39;,
        /// the leading N-by-N lower triangular part of B contains
        /// the lower triangular part of the matrix B.
        /// 
        /// On exit, if INFO &lt;= N, the part of B containing the matrix is
        /// overwritten by the triangular factor U or L from the Cholesky
        /// factorization B = U**T*U or B = L*L**T.
        /// </param>
        /// <param name="ldb">
        /// [in] LDB is INTEGER.
        /// The leading dimension of the array B.  LDB &gt;= max(1,N).
        /// </param>
        /// <param name="w">
        /// [out] W is REAL array, dimension (N).
        /// If INFO = 0, the eigenvalues in ascending order.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  SPOTRF or SSYEVD returned an error code:
        /// &lt;= N:  if INFO = i and JOBZ = &#39;N&#39;, then the algorithm
        /// failed to converge; i off-diagonal elements of an
        /// intermediate tridiagonal form did not converge to
        /// zero;
        /// if INFO = i and JOBZ = &#39;V&#39;, then the algorithm
        /// failed to compute an eigenvalue while working on
        /// the submatrix lying in rows and columns INFO/(N+1)
        /// through mod(INFO,N+1);
        /// &gt; N:   if INFO = N + i, for 1 &lt;= i &lt;= N, then the leading
        /// principal minor of order i of B is not positive.
        /// The factorization of B could not be completed and
        /// no eigenvalues or eigenvectors were computed.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  Modified so that no backsubstitution is performed if SSYEVD fails to
        ///  converge (NEIG in old code could be greater than N causing out of
        ///  bounds reference to A - reported by Ralf Meyer).  Also corrected the
        ///  description of INFO and the test on ITYPE. Sven, 16 Feb 05.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ssygvd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ssygvd(
            MatrixLayout matrixLayout,
            int itype,
            char jobz,
            char uplo,
            int n,
            float* a,
            int lda,
            float* b,
            int ldb,
            float* w);
    }
}
