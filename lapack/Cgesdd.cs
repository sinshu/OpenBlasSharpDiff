using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGESDD computes the singular value decomposition (SVD) of a complex
        /// M-by-N matrix A, optionally computing the left and/or right singular
        /// vectors, by using divide-and-conquer method. The SVD is written
        /// </para>
        /// <para>
        ///      A = U * SIGMA * conjugate-transpose(V)
        /// </para>
        /// <para>
        /// where SIGMA is an M-by-N matrix which is zero except for its
        /// min(m,n) diagonal elements, U is an M-by-M unitary matrix, and
        /// V is an N-by-N unitary matrix.  The diagonal elements of SIGMA
        /// are the singular values of A; they are real and non-negative, and
        /// are returned in descending order.  The first min(m,n) columns of
        /// U and V are the left and right singular vectors of A.
        /// </para>
        /// <para>
        /// Note that the routine returns VT = V**H, not V.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobz">
        /// [in] JOBZ is CHARACTER*1.
        /// Specifies options for computing all or part of the matrix U:
        /// = &#39;A&#39;:  all M columns of U and all N rows of V**H are
        /// returned in the arrays U and VT;
        /// = &#39;S&#39;:  the first min(M,N) columns of U and the first
        /// min(M,N) rows of V**H are returned in the arrays U
        /// and VT;
        /// = &#39;O&#39;:  If M &gt;= N, the first N columns of U are overwritten
        /// in the array A and all rows of V**H are returned in
        /// the array VT;
        /// otherwise, all columns of U are returned in the
        /// array U and the first M rows of V**H are overwritten
        /// in the array A;
        /// = &#39;N&#39;:  no columns of U or rows of V**H are computed.
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
        /// [in,out] A is COMPLEX array, dimension (LDA,N).
        /// On entry, the M-by-N matrix A.
        /// On exit,
        /// if JOBZ = &#39;O&#39;,  A is overwritten with the first N columns
        /// of U (the left singular vectors, stored
        /// columnwise) if M &gt;= N;
        /// A is overwritten with the first M rows
        /// of V**H (the right singular vectors, stored
        /// rowwise) otherwise.
        /// if JOBZ .ne. &#39;O&#39;, the contents of A are destroyed.
        /// </param>
        /// <param name="lda">
        /// [in] LDA is INTEGER.
        /// The leading dimension of the array A.  LDA &gt;= max(1,M).
        /// </param>
        /// <param name="s">
        /// [out] S is REAL array, dimension (min(M,N)).
        /// The singular values of A, sorted so that S(i) &gt;= S(i+1).
        /// </param>
        /// <param name="u">
        /// [out] U is COMPLEX array, dimension (LDU,UCOL).
        /// UCOL = M if JOBZ = &#39;A&#39; or JOBZ = &#39;O&#39; and M &lt; N;
        /// UCOL = min(M,N) if JOBZ = &#39;S&#39;.
        /// If JOBZ = &#39;A&#39; or JOBZ = &#39;O&#39; and M &lt; N, U contains the M-by-M
        /// unitary matrix U;
        /// if JOBZ = &#39;S&#39;, U contains the first min(M,N) columns of U
        /// (the left singular vectors, stored columnwise);
        /// if JOBZ = &#39;O&#39; and M &gt;= N, or JOBZ = &#39;N&#39;, U is not referenced.
        /// </param>
        /// <param name="ldu">
        /// [in] LDU is INTEGER.
        /// The leading dimension of the array U.  LDU &gt;= 1;
        /// if JOBZ = &#39;S&#39; or &#39;A&#39; or JOBZ = &#39;O&#39; and M &lt; N, LDU &gt;= M.
        /// </param>
        /// <param name="vt">
        /// [out] VT is COMPLEX array, dimension (LDVT,N).
        /// If JOBZ = &#39;A&#39; or JOBZ = &#39;O&#39; and M &gt;= N, VT contains the
        /// N-by-N unitary matrix V**H;
        /// if JOBZ = &#39;S&#39;, VT contains the first min(M,N) rows of
        /// V**H (the right singular vectors, stored rowwise);
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
        /// &gt; 0:  The updating process of SBDSDC did not converge.
        /// =  0:  successful exit.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgesdd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgesdd(
            MatrixLayout matrixLayout,
            char jobz,
            int m,
            int n,
            Complex32* a,
            int lda,
            float* s,
            Complex32* u,
            int ldu,
            Complex32* vt,
            int ldvt);
    }
}
