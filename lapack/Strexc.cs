using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// STREXC reorders the real Schur factorization of a real matrix
        /// A = Q*T*Q**T, so that the diagonal block of T with row index IFST is
        /// moved to row ILST.
        /// </para>
        /// <para>
        /// The real Schur form T is reordered by an orthogonal similarity
        /// transformation Z**T*T*Z, and optionally the matrix Q of Schur vectors
        /// is updated by postmultiplying it with Z.
        /// </para>
        /// <para>
        /// T must be in Schur canonical form (as returned by SHSEQR), that is,
        /// block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
        /// 2-by-2 diagonal block has its diagonal elements equal and its
        /// off-diagonal elements of opposite sign.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="compq">
        /// [in] COMPQ is CHARACTER*1.
        /// = &#39;V&#39;:  update the matrix Q of Schur vectors;
        /// = &#39;N&#39;:  do not update Q.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix T. N &gt;= 0.
        /// If N == 0 arguments ILST and IFST may be any value.
        /// </param>
        /// <param name="t">
        /// [in,out] T is REAL array, dimension (LDT,N).
        /// On entry, the upper quasi-triangular matrix T, in Schur
        /// Schur canonical form.
        /// On exit, the reordered upper quasi-triangular matrix, again
        /// in Schur canonical form.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T. LDT &gt;= max(1,N).
        /// </param>
        /// <param name="q">
        /// [in,out] Q is REAL array, dimension (LDQ,N).
        /// On entry, if COMPQ = &#39;V&#39;, the matrix Q of Schur vectors.
        /// On exit, if COMPQ = &#39;V&#39;, Q has been postmultiplied by the
        /// orthogonal transformation matrix Z which reorders T.
        /// If COMPQ = &#39;N&#39;, Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q.  LDQ &gt;= 1, and if
        /// COMPQ = &#39;V&#39;, LDQ &gt;= max(1,N).
        /// </param>
        /// <param name="ifst">
        /// [in,out] IFST is INTEGER.
        /// </param>
        /// <param name="ilst">
        /// [in,out] ILST is INTEGER.
        /// 
        /// Specify the reordering of the diagonal blocks of T.
        /// The block with row index IFST is moved to row ILST, by a
        /// sequence of transpositions between adjacent blocks.
        /// On exit, if IFST pointed on entry to the second row of a
        /// 2-by-2 block, it is changed to point to the first row; ILST
        /// always points to the first row of the block in its final
        /// position (which may differ from its input value by +1 or -1).
        /// 1 &lt;= IFST &lt;= N; 1 &lt;= ILST &lt;= N.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// = 1:  two adjacent blocks were too close to swap (the problem
        /// is very ill-conditioned); T may have been partially
        /// reordered, and ILST points to the first row of the
        /// current position of the block being moved.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_strexc", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Strexc(
            MatrixLayout matrixLayout,
            char compq,
            int n,
            float* t,
            int ldt,
            float* q,
            int ldq,
            int* ifst,
            int* ilst);
    }
}
