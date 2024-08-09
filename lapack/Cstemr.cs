using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CSTEMR computes selected eigenvalues and, optionally, eigenvectors
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
        /// Depending on the number of desired eigenvalues, these are computed either
        /// by bisection or the dqds algorithm. Numerically orthogonal eigenvectors are
        /// computed by the use of various suitable L D L^T factorizations near clusters
        /// of close eigenvalues (referred to as RRRs, Relatively Robust
        /// Representations). An informal sketch of the algorithm follows.
        /// </para>
        /// <para>
        /// For each unreduced block (submatrix) of T,
        ///    (a) Compute T - sigma I  = L D L^T, so that L and D
        ///        define all the wanted eigenvalues to high relative accuracy.
        ///        This means that small relative changes in the entries of D and L
        ///        cause only small relative changes in the eigenvalues and
        ///        eigenvectors. The standard (unfactored) representation of the
        ///        tridiagonal matrix T does not have this property in general.
        ///    (b) Compute the eigenvalues to suitable accuracy.
        ///        If the eigenvectors are desired, the algorithm attains full
        ///        accuracy of the computed eigenvalues only right before
        ///        the corresponding vectors have to be computed, see steps c) and d).
        ///    (c) For each cluster of close eigenvalues, select a new
        ///        shift close to the cluster, find a new factorization, and refine
        ///        the shifted eigenvalues to suitable accuracy.
        ///    (d) For each eigenvalue with a large enough relative separation compute
        ///        the corresponding eigenvector by forming a rank revealing twisted
        ///        factorization. Go back to (c) for any clusters that remain.
        /// </para>
        /// <para>
        /// For more details, see:
        /// - Inderjit S. Dhillon and Beresford N. Parlett: &quot;Multiple representations
        ///   to compute orthogonal eigenvectors of symmetric tridiagonal matrices,&quot;
        ///   Linear Algebra and its Applications, 387(1), pp. 1-28, August 2004.
        /// - Inderjit Dhillon and Beresford Parlett: &quot;Orthogonal Eigenvectors and
        ///   Relative Gaps,&quot; SIAM Journal on Matrix Analysis and Applications, Vol. 25,
        ///   2004.  Also LAPACK Working Note 154.
        /// - Inderjit Dhillon: &quot;A new O(n^2) algorithm for the symmetric
        ///   tridiagonal eigenvalue/eigenvector problem&quot;,
        ///   Computer Science Division Technical Report No. UCB/CSD-97-971,
        ///   UC Berkeley, May 1997.
        /// </para>
        /// <para>
        /// Further Details
        /// 1.CSTEMR works only on machines which follow IEEE-754
        /// floating-point standard in their handling of infinities and NaNs.
        /// This permits the use of efficient inner loops avoiding a check for
        /// zero divisors.
        /// </para>
        /// <para>
        /// 2. LAPACK routines can be used to reduce a complex Hermitean matrix to
        /// real symmetric tridiagonal form.
        /// </para>
        /// <para>
        /// (Any complex Hermitean tridiagonal matrix has real values on its diagonal
        /// and potentially complex numbers on its off-diagonals. By applying a
        /// similarity transform with an appropriate diagonal matrix
        /// diag(1,e^{i \phy_1}, ... , e^{i \phy_{n-1}}), the complex Hermitean
        /// matrix can be transformed into a real symmetric matrix and complex
        /// arithmetic can be entirely avoided.)
        /// </para>
        /// <para>
        /// While the eigenvectors of the real symmetric tridiagonal matrix are real,
        /// the eigenvectors of original complex Hermitean matrix have complex entries
        /// in general.
        /// Since LAPACK drivers overwrite the matrix data with the eigenvectors,
        /// CSTEMR accepts complex workspace to facilitate interoperability
        /// with CUNMTR or CUPMTR.
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
        /// is not known in advance and can be computed with a workspace
        /// query by setting NZC = -1, see below.
        /// </param>
        /// <param name="ldz">
        /// [in] LDZ is INTEGER.
        /// The leading dimension of the array Z.  LDZ &gt;= 1, and if
        /// JOBZ = &#39;V&#39;, then LDZ &gt;= max(1,N).
        /// </param>
        /// <param name="nzc">
        /// [in] NZC is INTEGER.
        /// The number of eigenvectors to be held in the array Z.
        /// If RANGE = &#39;A&#39;, then NZC &gt;= max(1,N).
        /// If RANGE = &#39;V&#39;, then NZC &gt;= the number of eigenvalues in (VL,VU].
        /// If RANGE = &#39;I&#39;, then NZC &gt;= IU-IL+1.
        /// If NZC = -1, then a workspace query is assumed; the
        /// routine calculates the number of columns of the array Z that
        /// are needed to hold the eigenvectors.
        /// This value is returned as the first entry of the Z array, and
        /// no error message related to NZC is issued by XERBLA.
        /// </param>
        /// <param name="isuppz">
        /// [out] ISUPPZ is INTEGER array, dimension ( 2*max(1,M) ).
        /// The support of the eigenvectors in Z, i.e., the indices
        /// indicating the nonzero elements in Z. The i-th computed eigenvector
        /// is nonzero only in elements ISUPPZ( 2*i-1 ) through
        /// ISUPPZ( 2*i ). This is relevant in the case when the matrix
        /// is split. ISUPPZ is only accessed when JOBZ is &#39;V&#39; and N &gt; 0.
        /// </param>
        /// <param name="tryrac">
        /// [in,out] TRYRAC is LOGICAL.
        /// If TRYRAC = .TRUE., indicates that the code should check whether
        /// the tridiagonal matrix defines its eigenvalues to high relative
        /// accuracy.  If so, the code uses relative-accuracy preserving
        /// algorithms that might be (a bit) slower depending on the matrix.
        /// If the matrix does not define its eigenvalues to high relative
        /// accuracy, the code can uses possibly faster algorithms.
        /// If TRYRAC = .FALSE., the code is not required to guarantee
        /// relatively accurate eigenvalues and can use the fastest possible
        /// techniques.
        /// On exit, a .TRUE. TRYRAC will be set to .FALSE. if the matrix
        /// does not define its eigenvalues to high relative accuracy.
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_cstemr", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Cstemr(
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
            int* m,
            float* w,
            Complex32* z,
            int ldz,
            int nzc,
            int* isuppz,
            bool* tryrac);
    }
}
