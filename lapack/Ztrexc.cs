using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZTREXC reorders the Schur factorization of a complex matrix
        /// A = Q*T*Q**H, so that the diagonal element of T with row index IFST
        /// is moved to row ILST.
        /// </para>
        /// <para>
        /// The Schur form T is reordered by a unitary similarity transformation
        /// Z**H*T*Z, and optionally the matrix Q of Schur vectors is updated by
        /// postmultiplying it with Z.
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
        /// [in,out] T is COMPLEX*16 array, dimension (LDT,N).
        /// On entry, the upper triangular matrix T.
        /// On exit, the reordered upper triangular matrix.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T. LDT &gt;= max(1,N).
        /// </param>
        /// <param name="q">
        /// [in,out] Q is COMPLEX*16 array, dimension (LDQ,N).
        /// On entry, if COMPQ = &#39;V&#39;, the matrix Q of Schur vectors.
        /// On exit, if COMPQ = &#39;V&#39;, Q has been postmultiplied by the
        /// unitary transformation matrix Z which reorders T.
        /// If COMPQ = &#39;N&#39;, Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q.  LDQ &gt;= 1, and if
        /// COMPQ = &#39;V&#39;, LDQ &gt;= max(1,N).
        /// </param>
        /// <param name="ifst">
        /// [in] IFST is INTEGER.
        /// </param>
        /// <param name="ilst">
        /// [in] ILST is INTEGER.
        /// 
        /// Specify the reordering of the diagonal elements of T:
        /// The element with row index IFST is moved to row ILST by a
        /// sequence of transpositions between adjacent elements.
        /// 1 &lt;= IFST &lt;= N; 1 &lt;= ILST &lt;= N.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ztrexc", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ztrexc(
            MatrixLayout matrixLayout,
            char compq,
            int n,
            Complex* t,
            int ldt,
            Complex* q,
            int ldq,
            int ifst,
            int ilst);
    }
}
