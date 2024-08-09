using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// SGBBRD reduces a real general m-by-n band matrix A to upper
        /// bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
        /// </para>
        /// <para>
        /// The routine computes B, and optionally forms Q or P**T, or computes
        /// Q**T*C for a given matrix C.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="vect">
        /// [in] VECT is CHARACTER*1.
        /// Specifies whether or not the matrices Q and P**T are to be
        /// formed.
        /// = &#39;N&#39;: do not form Q or P**T;
        /// = &#39;Q&#39;: form Q only;
        /// = &#39;P&#39;: form P**T only;
        /// = &#39;B&#39;: form both.
        /// </param>
        /// <param name="m">
        /// [in] M is INTEGER.
        /// The number of rows of the matrix A.  M &gt;= 0.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The number of columns of the matrix A.  N &gt;= 0.
        /// </param>
        /// <param name="ncc">
        /// [in] NCC is INTEGER.
        /// The number of columns of the matrix C.  NCC &gt;= 0.
        /// </param>
        /// <param name="kl">
        /// [in] KL is INTEGER.
        /// The number of subdiagonals of the matrix A. KL &gt;= 0.
        /// </param>
        /// <param name="ku">
        /// [in] KU is INTEGER.
        /// The number of superdiagonals of the matrix A. KU &gt;= 0.
        /// </param>
        /// <param name="ab">
        /// [in,out] AB is REAL array, dimension (LDAB,N).
        /// On entry, the m-by-n band matrix A, stored in rows 1 to
        /// KL+KU+1. The j-th column of A is stored in the j-th column of
        /// the array AB as follows:
        /// AB(ku+1+i-j,j) = A(i,j) for max(1,j-ku)&lt;=i&lt;=min(m,j+kl).
        /// On exit, A is overwritten by values generated during the
        /// reduction.
        /// </param>
        /// <param name="ldab">
        /// [in] LDAB is INTEGER.
        /// The leading dimension of the array A. LDAB &gt;= KL+KU+1.
        /// </param>
        /// <param name="d">
        /// [out] D is REAL array, dimension (min(M,N)).
        /// The diagonal elements of the bidiagonal matrix B.
        /// </param>
        /// <param name="e">
        /// [out] E is REAL array, dimension (min(M,N)-1).
        /// The superdiagonal elements of the bidiagonal matrix B.
        /// </param>
        /// <param name="q">
        /// [out] Q is REAL array, dimension (LDQ,M).
        /// If VECT = &#39;Q&#39; or &#39;B&#39;, the m-by-m orthogonal matrix Q.
        /// If VECT = &#39;N&#39; or &#39;P&#39;, the array Q is not referenced.
        /// </param>
        /// <param name="ldq">
        /// [in] LDQ is INTEGER.
        /// The leading dimension of the array Q.
        /// LDQ &gt;= max(1,M) if VECT = &#39;Q&#39; or &#39;B&#39;; LDQ &gt;= 1 otherwise.
        /// </param>
        /// <param name="pt">
        /// [out] PT is REAL array, dimension (LDPT,N).
        /// If VECT = &#39;P&#39; or &#39;B&#39;, the n-by-n orthogonal matrix P&#39;.
        /// If VECT = &#39;N&#39; or &#39;Q&#39;, the array PT is not referenced.
        /// </param>
        /// <param name="ldpt">
        /// [in] LDPT is INTEGER.
        /// The leading dimension of the array PT.
        /// LDPT &gt;= max(1,N) if VECT = &#39;P&#39; or &#39;B&#39;; LDPT &gt;= 1 otherwise.
        /// </param>
        /// <param name="c">
        /// [in,out] C is REAL array, dimension (LDC,NCC).
        /// On entry, an m-by-ncc matrix C.
        /// On exit, C is overwritten by Q**T*C.
        /// C is not referenced if NCC = 0.
        /// </param>
        /// <param name="ldc">
        /// [in] LDC is INTEGER.
        /// The leading dimension of the array C.
        /// LDC &gt;= max(1,M) if NCC &gt; 0; LDC &gt;= 1 if NCC = 0.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_sgbbrd", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Sgbbrd(
            MatrixLayout matrixLayout,
            char vect,
            int m,
            int n,
            int ncc,
            int kl,
            int ku,
            float* ab,
            int ldab,
            float* d,
            float* e,
            float* q,
            int ldq,
            float* pt,
            int ldpt,
            float* c,
            int ldc);
    }
}
