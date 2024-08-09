using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CSTEGR computes selected eigenvalues and, optionally, eigenvectors
        /// of a real symmetric tridiagonal matrix T. Any such unreduced matrix has
        /// a well defined set of pairwise different real eigenvalues, the corresponding
        /// real eigenvectors are pairwise orthogonal.
        /// </para>
        /// <para>
        /// The spectrum may be computed either completely or partially by specifying
        /// either an interval (VL,VU] or a range of indices IL:IU for the desired
        /// eigenvalues.
        /// </para>
        /// <para>
        /// CSTEGR is a compatibility wrapper around the improved CSTEMR routine.
        /// See SSTEMR for further details.
        /// </para>
        /// <para>
        /// One important change is that the ABSTOL parameter no longer provides any
        /// benefit and hence is no longer used.
        /// </para>
        /// <para>
        /// Note : CSTEGR and CSTEMR work only on machines which follow
        /// IEEE-754 floating-point standard in their handling of infinities and
        /// NaNs.  Normal execution may create these exceptional values and hence
        /// may abort due to a floating point exception in environments which
        /// do not conform to the IEEE-754 standard.
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
        /// <param name="range">
        /// [in] RANGE is CHARACTER*1.
        /// = &#39;A&#39;: all eigenvalues will be found.
        /// = &#39;V&#39;: all eigenvalues in the half-open interval (VL,VU]
        /// will be found.
        /// = &#39;I&#39;: the IL-th through IU-th eigenvalues will be found.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix.  N &gt;= 0.
        /// </param>
        /// <param name="d">
        /// [in,out] D is REAL array, dimension (N).
        /// On entry, the N diagonal elements of the tridiagonal matrix
        /// T. On exit, D is overwritten.
        /// </param>
        /// <param name="e">
        /// [in,out] E is REAL array, dimension (N).
        /// On entry, the (N-1) subdiagonal elements of the tridiagonal
        /// matrix T in elements 1 to N-1 of E. E(N) need not be set on
        /// input, but is used internally as workspace.
        /// On exit, E is overwritten.
        /// </param>
        /// <param name="vl">
        /// [in] VL is REAL.
        /// 
        /// If RANGE=&#39;V&#39;, the lower bound of the interval to
        /// be searched for eigenvalues. VL &lt; VU.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="vu">
        /// [in] VU is REAL.
        /// 
        /// If RANGE=&#39;V&#39;, the upper bound of the interval to
        /// be searched for eigenvalues. VL &lt; VU.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;I&#39;.
        /// </param>
        /// <param name="il">
        /// [in] IL is INTEGER.
        /// 
        /// If RANGE=&#39;I&#39;, the index of the
        /// smallest eigenvalue to be returned.
        /// 1 &lt;= IL &lt;= IU &lt;= N, if N &gt; 0.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;V&#39;.
        /// </param>
        /// <param name="iu">
        /// [in] IU is INTEGER.
        /// 
        /// If RANGE=&#39;I&#39;, the index of the
        /// largest eigenvalue to be returned.
        /// 1 &lt;= IL &lt;= IU &lt;= N, if N &gt; 0.
        /// Not referenced if RANGE = &#39;A&#39; or &#39;V&#39;.
        /// </param>
        /// <param name="abstol">
        /// [in] ABSTOL is REAL.
        /// Unused.  Was the absolute error tolerance for the
        /// eigenvalues/eigenvectors in previous versions.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The total number of eigenvalues found.  0 &lt;= M &lt;= N.
        /// If RANGE = &#39;A&#39;, M = N, and if RANGE = &#39;I&#39;, M = IU-IL+1.
        /// </param>
        /// <param name="w">
        /// [out] W is REAL array, dimension (N).
        /// The first M elements contain the selected eigenvalues in
        /// ascending order.
        /// </param>
        /// <param name="z">
        /// [out] Z is COMPLEX array, dimension (LDZ, max(1,M) ).
        /// If JOBZ = &#39;V&#39;, and if INFO = 0, then the first M columns of Z
        /// contain the orthonormal eigenvectors of the matrix T
        /// corresponding to the selected eigenvalues, with the i-th
        /// column of Z holding the eigenvector associated with W(i).
        /// If JOBZ = &#39;N&#39;, then Z is not referenced.
        /// Note: the user must ensure that at least max(1,M) columns are
        /// supplied in the array Z; if RANGE = &#39;V&#39;, the exact value of M
        /// is not known in advance and an upper bound must be used.
        /// Supplying N columns is always safe.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// JOBZ = &#39;V&#39;, then LDZ &gt;= max(1,N).
        /// </param>
        /// <param name="isuppz">
        /// [out] ISUPPZ is INTEGER array, dimension ( 2*max(1,M) ).
        /// The support of the eigenvectors in Z, i.e., the indices
        /// indicating the nonzero elements in Z. The i-th computed eigenvector
        /// is nonzero only in elements ISUPPZ( 2*i-1 ) through
        /// ISUPPZ( 2*i ). This is relevant in the case when the matrix
        /// is split. ISUPPZ is only accessed when JOBZ is &#39;V&#39; and N &gt; 0.
        /// </param>
        /// <returns>
        /// On exit, INFO
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = 1X, internal error in SLARRE,
        /// if INFO = 2X, internal error in CLARRV.
        /// Here, the digit X = ABS( IINFO ) &lt; 10, where IINFO is
        /// the nonzero error code returned by SLARRE or
        /// CLARRV, respectively.
        /// </returns>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cstegr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cstegr(
            MatrixLayout matrixLayout,
            char jobz,
            char range,
            int n,
            float* d,
            float* e,
            float vl,
            float vu,
            int il,
            int iu,
            float abstol,
            int* m,
            float* w,
            Complex32* z,
            int ldz,
            int* isuppz);
    }
}
