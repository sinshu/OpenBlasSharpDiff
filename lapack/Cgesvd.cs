using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CGESVD computes the singular value decomposition (SVD) of a complex
        /// M-by-N matrix A, optionally computing the left and/or right singular
        /// vectors. The SVD is written
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
        /// Note that the routine returns V**H, not V.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="jobu">
        /// [in] JOBU is CHARACTER*1.
        /// Specifies options for computing all or part of the matrix U:
        /// = &#39;A&#39;:  all M columns of U are returned in array U:
        /// = &#39;S&#39;:  the first min(m,n) columns of U (the left singular
        /// vectors) are returned in the array U;
        /// = &#39;O&#39;:  the first min(m,n) columns of U (the left singular
        /// vectors) are overwritten on the array A;
        /// = &#39;N&#39;:  no columns of U (no left singular vectors) are
        /// computed.
        /// </param>
        /// <param name="jobvt">
        /// [in] JOBVT is CHARACTER*1.
        /// Specifies options for computing all or part of the matrix
        /// V**H:
        /// = &#39;A&#39;:  all N rows of V**H are returned in the array VT;
        /// = &#39;S&#39;:  the first min(m,n) rows of V**H (the right singular
        /// vectors) are returned in the array VT;
        /// = &#39;O&#39;:  the first min(m,n) rows of V**H (the right singular
        /// vectors) are overwritten on the array A;
        /// = &#39;N&#39;:  no rows of V**H (no right singular vectors) are
        /// computed.
        /// 
        /// JOBVT and JOBU cannot both be &#39;O&#39;.
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
        /// if JOBU = &#39;O&#39;,  A is overwritten with the first min(m,n)
        /// columns of U (the left singular vectors,
        /// stored columnwise);
        /// if JOBVT = &#39;O&#39;, A is overwritten with the first min(m,n)
        /// rows of V**H (the right singular vectors,
        /// stored rowwise);
        /// if JOBU .ne. &#39;O&#39; and JOBVT .ne. &#39;O&#39;, the contents of A
        /// are destroyed.
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
        /// (LDU,M) if JOBU = &#39;A&#39; or (LDU,min(M,N)) if JOBU = &#39;S&#39;.
        /// If JOBU = &#39;A&#39;, U contains the M-by-M unitary matrix U;
        /// if JOBU = &#39;S&#39;, U contains the first min(m,n) columns of U
        /// (the left singular vectors, stored columnwise);
        /// if JOBU = &#39;N&#39; or &#39;O&#39;, U is not referenced.
        /// </param>
        /// <param name="ldu">
        /// [in] LDU is INTEGER.
        /// The leading dimension of the array U.  LDU &gt;= 1; if
        /// JOBU = &#39;S&#39; or &#39;A&#39;, LDU &gt;= M.
        /// </param>
        /// <param name="vt">
        /// [out] VT is COMPLEX array, dimension (LDVT,N).
        /// If JOBVT = &#39;A&#39;, VT contains the N-by-N unitary matrix
        /// V**H;
        /// if JOBVT = &#39;S&#39;, VT contains the first min(m,n) rows of
        /// V**H (the right singular vectors, stored rowwise);
        /// if JOBVT = &#39;N&#39; or &#39;O&#39;, VT is not referenced.
        /// </param>
        /// <param name="ldvt">
        /// [in] LDVT is INTEGER.
        /// The leading dimension of the array VT.  LDVT &gt;= 1; if
        /// JOBVT = &#39;A&#39;, LDVT &gt;= N; if JOBVT = &#39;S&#39;, LDVT &gt;= min(M,N).
        /// </param>
        /// <param name="superb">
        /// No description available.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  if CBDSQR did not converge, INFO specifies how many
        /// superdiagonals of an intermediate bidiagonal form B
        /// did not converge to zero. See the description of RWORK
        /// above for details.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cgesvd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cgesvd(
            MatrixLayout matrixLayout,
            char jobu,
            char jobvt,
            int m,
            int n,
            Complex32* a,
            int lda,
            float* s,
            Complex32* u,
            int ldu,
            Complex32* vt,
            int ldvt,
            float* superb);
    }
}
