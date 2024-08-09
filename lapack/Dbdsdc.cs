using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// DBDSDC computes the singular value decomposition (SVD) of a real
        /// N-by-N (upper or lower) bidiagonal matrix B:  B = U * S * VT,
        /// using a divide and conquer method, where S is a diagonal matrix
        /// with non-negative diagonal elements (the singular values of B), and
        /// U and VT are orthogonal matrices of left and right singular vectors,
        /// respectively. DBDSDC can be used to compute all singular values,
        /// and optionally, singular vectors or singular vectors in compact form.
        /// </para>
        /// <para>
        /// The code currently calls DLASDQ if singular values only are desired.
        /// However, it can be slightly modified to compute singular values
        /// using the divide and conquer method.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="uplo">
        /// [in] UPLO is CHARACTER*1.
        /// = &#39;U&#39;:  B is upper bidiagonal.
        /// = &#39;L&#39;:  B is lower bidiagonal.
        /// </param>
        /// <param name="compq">
        /// [in] COMPQ is CHARACTER*1.
        /// Specifies whether singular vectors are to be computed
        /// as follows:
        /// = &#39;N&#39;:  Compute singular values only;
        /// = &#39;P&#39;:  Compute singular values and compute singular
        /// vectors in compact form;
        /// = &#39;I&#39;:  Compute singular values and singular vectors.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix B.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in,out] D is DOUBLE PRECISION array, dimension (N).
        /// On entry, the n diagonal elements of the bidiagonal matrix B.
        /// On exit, if INFO=0, the singular values of B.
        /// </param>
        /// <param name="e">
        /// [in,out] E is DOUBLE PRECISION array, dimension (N-1).
        /// On entry, the elements of E contain the offdiagonal
        /// elements of the bidiagonal matrix whose SVD is desired.
        /// On exit, E has been destroyed.
        /// </param>
        /// <param name="u">
        /// [out] U is DOUBLE PRECISION array, dimension (LDU,N).
        /// If  COMPQ = &#39;I&#39;, then:
        /// On exit, if INFO = 0, U contains the left singular vectors
        /// of the bidiagonal matrix.
        /// For other values of COMPQ, U is not referenced.
        /// </param>
        /// <param name="ldu">
        /// [in] LDU is INTEGER.
        /// The leading dimension of the array U.  LDU &gt;= 1.
        /// If singular vectors are desired, then LDU &gt;= max( 1, N ).
        /// </param>
        /// <param name="vt">
        /// [out] VT is DOUBLE PRECISION array, dimension (LDVT,N).
        /// If  COMPQ = &#39;I&#39;, then:
        /// On exit, if INFO = 0, VT**T contains the right singular
        /// vectors of the bidiagonal matrix.
        /// For other values of COMPQ, VT is not referenced.
        /// </param>
        /// <param name="ldvt">
        /// [in] LDVT is INTEGER.
        /// The leading dimension of the array VT.  LDVT &gt;= 1.
        /// If singular vectors are desired, then LDVT &gt;= max( 1, N ).
        /// </param>
        /// <param name="q">
        /// [out] Q is DOUBLE PRECISION array, dimension (LDQ).
        /// If  COMPQ = &#39;P&#39;, then:
        /// On exit, if INFO = 0, Q and IQ contain the left
        /// and right singular vectors in a compact form,
        /// requiring O(N log N) space instead of 2*N**2.
        /// In particular, Q contains all the DOUBLE PRECISION data in
        /// LDQ &gt;= N*(11 + 2*SMLSIZ + 8*INT(LOG_2(N/(SMLSIZ+1))))
        /// words of memory, where SMLSIZ is returned by ILAENV and
        /// is equal to the maximum size of the subproblems at the
        /// bottom of the computation tree (usually about 25).
        /// For other values of COMPQ, Q is not referenced.
        /// </param>
        /// <param name="iq">
        /// [out] IQ is INTEGER array, dimension (LDIQ).
        /// If  COMPQ = &#39;P&#39;, then:
        /// On exit, if INFO = 0, Q and IQ contain the left
        /// and right singular vectors in a compact form,
        /// requiring O(N log N) space instead of 2*N**2.
        /// In particular, IQ contains all INTEGER data in
        /// LDIQ &gt;= N*(3 + 3*INT(LOG_2(N/(SMLSIZ+1))))
        /// words of memory, where SMLSIZ is returned by ILAENV and
        /// is equal to the maximum size of the subproblems at the
        /// bottom of the computation tree (usually about 25).
        /// For other values of COMPQ, IQ is not referenced.
        /// </param>
        /// <returns>
        /// = 0:  successful exit.
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value.
        /// &gt; 0:  The algorithm failed to compute a singular value.
        /// The update process of divide and conquer failed.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_dbdsdc", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Dbdsdc(
            MatrixLayout matrixLayout,
            char uplo,
            char compq,
            int n,
            double* d,
            double* e,
            double* u,
            int ldu,
            double* vt,
            int ldvt,
            double* q,
            int* iq);
    }
}
