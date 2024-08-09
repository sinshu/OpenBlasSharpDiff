using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DGESDD computes the singular value decomposition (SVD) of a real
        /// M-by-N matrix A, optionally computing the left and right singular
        /// vectors.  If singular vectors are desired, it uses a
        /// divide-and-conquer algorithm.
        /// </para>
        /// <para>
        /// The SVD is written
        /// </para>
        /// <para>
        ///      A = U * SIGMA * transpose(V)
        /// </para>
        /// <para>
        /// where SIGMA is an M-by-N matrix which is zero except for its
        /// min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
        /// V is an N-by-N orthogonal matrix.  The diagonal elements of SIGMA
        /// are the singular values of A; they are real and non-negative, and
        /// are returned in descending order.  The first min(m,n) columns of
        /// U and V are the left and right singular vectors of A.
        /// </para>
        /// <para>
        /// Note that the routine returns VT = V**T, not V.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobz">
        /// [in] JOBZ is CHARACTER*1.
        /// Specifies options for computing all or part of the matrix U:
        /// = &#39;A&#39;:  all M columns of U and all N rows of V**T are
        /// returned in the arrays U and VT;
        /// = &#39;S&#39;:  the first min(M,N) columns of U and the first
        /// min(M,N) rows of V**T are returned in the arrays U
        /// and VT;
        /// = &#39;O&#39;:  If M &gt;= N, the first N columns of U are overwritten
        /// on the array A and all rows of V**T are returned in
        /// the array VT;
        /// otherwise, all columns of U are returned in the
        /// array U and the first M rows of V**T are overwritten
        /// in the array A;
        /// = &#39;N&#39;:  no columns of U or rows of V**T are computed.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the input matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the input matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="a">
        /// [in,out] A is DOUBLE PRECISION array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit,
        /// if JOBZ = &#39;O&#39;,  A is overwritten with the first N columns
        /// of U (the left singular vectors, stored
        /// columnwise) if M &gt;= N;
        /// A is overwritten with the first M rows
        /// of V**T (the right singular vectors, stored
        /// rowwise) otherwise.
        /// if JOBZ .ne. &#39;O&#39;, the contents of A are destroyed.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="s">
        /// [out] S is DOUBLE PRECISION array, dimension (min(M,N)).
        /// The singular values of A, sorted so that S(i) &gt;= S(i+1).
        /// </param>
        /// <param name="u">
        /// [out] U is DOUBLE PRECISION array, dimension (LDU,UCOL).
        /// UCOL = M if JOBZ = &#39;A&#39; or JOBZ = &#39;O&#39; and M &lt; N;
        /// UCOL = min(M,N) if JOBZ = &#39;S&#39;.
        /// If JOBZ = &#39;A&#39; or JOBZ = &#39;O&#39; and M &lt; N, U contains the M-by-M
        /// orthogonal matrix U;
        /// if JOBZ = &#39;S&#39;, U contains the first min(M,N) columns of U
        /// (the left singular vectors, stored columnwise);
        /// if JOBZ = &#39;O&#39; and M &gt;= N, or JOBZ = &#39;N&#39;, U is not referenced.
        /// </param>
        /// <param name="ldu">
        /// [in] LDU is INTEGER.
        /// The leading dimension of the array U.  LDU &gt;= 1; if
        /// JOBZ = &#39;S&#39; or &#39;A&#39; or JOBZ = &#39;O&#39; and M &lt; N, LDU &gt;= M.
        /// </param>
        /// <param name="vt">
        /// [out] VT is DOUBLE PRECISION array, dimension (LDVT,N).
        /// If JOBZ = &#39;A&#39; or JOBZ = &#39;O&#39; and M &gt;= N, VT contains the
        /// N-by-N orthogonal matrix V**T;
        /// if JOBZ = &#39;S&#39;, VT contains the first min(M,N) rows of
        /// V**T (the right singular vectors, stored rowwise);
        /// if JOBZ = &#39;O&#39; and M &lt; N, or JOBZ = &#39;N&#39;, VT is not referenced.
        /// </param>
        /// <param name="ldvt">
        /// [in] LDVT is INTEGER.
        /// The leading dimension of the array VT.  LDVT &gt;= 1;
        /// if JOBZ = &#39;A&#39; or JOBZ = &#39;O&#39; and M &gt;= N, LDVT &gt;= N;
        /// if JOBZ = &#39;S&#39;, LDVT &gt;= min(M,N).
        /// </param>
        /// <returns>
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// = -4:  if A had a NAN entry.
        /// &gt; 0:  DBDSDC did not converge, updating process failed.
        /// =  0:  successful exit.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dgesdd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dgesdd(
            MatrixLayout matrixLayout,
            char jobz,
            int m,
            int n,
            double* a,
            int lda,
            double* s,
            double* u,
            int ldu,
            double* vt,
            int ldvt);
    }
}
