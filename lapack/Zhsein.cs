using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// ZHSEIN uses inverse iteration to find specified right and/or left
        /// eigenvectors of a complex upper Hessenberg matrix H.
        /// </para>
        /// <para>
        /// The right eigenvector x and the left eigenvector y of the matrix H
        /// corresponding to an eigenvalue w are defined by:
        /// </para>
        /// <para>
        ///              H * x = w * x,     y**h * H = w * y**h
        /// </para>
        /// <para>
        /// where y**h denotes the conjugate transpose of the vector y.
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="job">
        /// No description available.
        /// </param>
        /// <param name="eigsrc">
        /// [in] EIGSRC is CHARACTER*1.
        /// Specifies the source of eigenvalues supplied in W:
        /// = &#39;Q&#39;: the eigenvalues were found using ZHSEQR; thus, if
        /// H has zero subdiagonal elements, and so is
        /// block-triangular, then the j-th eigenvalue can be
        /// assumed to be an eigenvalue of the block containing
        /// the j-th row/column.  This property allows ZHSEIN to
        /// perform inverse iteration on just one diagonal block.
        /// = &#39;N&#39;: no assumptions are made on the correspondence
        /// between eigenvalues and diagonal blocks.  In this
        /// case, ZHSEIN must always perform inverse iteration
        /// using the whole matrix H.
        /// </param>
        /// <param name="initv">
        /// [in] INITV is CHARACTER*1.
        /// = &#39;N&#39;: no initial vectors are supplied;
        /// = &#39;U&#39;: user-supplied initial vectors are stored in the arrays
        /// VL and/or VR.
        /// </param>
        /// <param name="select">
        /// [in] SELECT is LOGICAL array, dimension (N).
        /// Specifies the eigenvectors to be computed. To select the
        /// eigenvector corresponding to the eigenvalue W(j),
        /// SELECT(j) must be set to .TRUE..
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix H.  N &gt;= 0.
        /// </param>
        /// <param name="h">
        /// [in] H is COMPLEX*16 array, dimension (LDH,N).
        /// The upper Hessenberg matrix H.
        /// If a NaN is detected in H, the routine will return with INFO=-6.
        /// </param>
        /// <param name="ldh">
        /// [in] LDH is INTEGER.
        /// The leading dimension of the array H.  LDH &gt;= max(1,N).
        /// </param>
        /// <param name="w">
        /// [in,out] W is COMPLEX*16 array, dimension (N).
        /// On entry, the eigenvalues of H.
        /// On exit, the real parts of W may have been altered since
        /// close eigenvalues are perturbed slightly in searching for
        /// independent eigenvectors.
        /// </param>
        /// <param name="vl">
        /// [in,out] VL is COMPLEX*16 array, dimension (LDVL,MM).
        /// On entry, if INITV = &#39;U&#39; and SIDE = &#39;L&#39; or &#39;B&#39;, VL must
        /// contain starting vectors for the inverse iteration for the
        /// left eigenvectors; the starting vector for each eigenvector
        /// must be in the same column in which the eigenvector will be
        /// stored.
        /// On exit, if SIDE = &#39;L&#39; or &#39;B&#39;, the left eigenvectors
        /// specified by SELECT will be stored consecutively in the
        /// columns of VL, in the same order as their eigenvalues.
        /// If SIDE = &#39;R&#39;, VL is not referenced.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the array VL.
        /// LDVL &gt;= max(1,N) if SIDE = &#39;L&#39; or &#39;B&#39;; LDVL &gt;= 1 otherwise.
        /// </param>
        /// <param name="vr">
        /// [in,out] VR is COMPLEX*16 array, dimension (LDVR,MM).
        /// On entry, if INITV = &#39;U&#39; and SIDE = &#39;R&#39; or &#39;B&#39;, VR must
        /// contain starting vectors for the inverse iteration for the
        /// right eigenvectors; the starting vector for each eigenvector
        /// must be in the same column in which the eigenvector will be
        /// stored.
        /// On exit, if SIDE = &#39;R&#39; or &#39;B&#39;, the right eigenvectors
        /// specified by SELECT will be stored consecutively in the
        /// columns of VR, in the same order as their eigenvalues.
        /// If SIDE = &#39;L&#39;, VR is not referenced.
        /// </param>
        /// <param name="ldvr">
        /// [in] LDVR is INTEGER.
        /// The leading dimension of the array VR.
        /// LDVR &gt;= max(1,N) if SIDE = &#39;R&#39; or &#39;B&#39;; LDVR &gt;= 1 otherwise.
        /// </param>
        /// <param name="mm">
        /// [in] MM is INTEGER.
        /// The number of columns in the arrays VL and/or VR. MM &gt;= M.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The number of columns in the arrays VL and/or VR required to
        /// store the eigenvectors (= the number of .TRUE. elements in
        /// SELECT).
        /// </param>
        /// <param name="ifaill">
        /// [out] IFAILL is INTEGER array, dimension (MM).
        /// If SIDE = &#39;L&#39; or &#39;B&#39;, IFAILL(i) = j &gt; 0 if the left
        /// eigenvector in the i-th column of VL (corresponding to the
        /// eigenvalue w(j)) failed to converge; IFAILL(i) = 0 if the
        /// eigenvector converged satisfactorily.
        /// If SIDE = &#39;R&#39;, IFAILL is not referenced.
        /// </param>
        /// <param name="ifailr">
        /// [out] IFAILR is INTEGER array, dimension (MM).
        /// If SIDE = &#39;R&#39; or &#39;B&#39;, IFAILR(i) = j &gt; 0 if the right
        /// eigenvector in the i-th column of VR (corresponding to the
        /// eigenvalue w(j)) failed to converge; IFAILR(i) = 0 if the
        /// eigenvector converged satisfactorily.
        /// If SIDE = &#39;L&#39;, IFAILR is not referenced.
        /// </param>
        /// <returns>
        /// = 0:  successful exit
        /// &lt; 0:  if INFO = -i, the i-th argument had an illegal value
        /// &gt; 0:  if INFO = i, i is the number of eigenvectors which
        /// failed to converge; see IFAILL and IFAILR for further
        /// details.
        /// </returns>
        /// <remarks>
        /// <para>
        ///  Each eigenvector is normalized so that the element of largest
        ///  magnitude has magnitude 1; here the magnitude of a complex number
        ///  (x,y) is taken to be |x|+|y|.
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_zhsein", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Zhsein(
            MatrixLayout matrixLayout,
            char job,
            char eigsrc,
            char initv,
            bool* select,
            int n,
            Complex* h,
            int ldh,
            Complex* w,
            Complex* vl,
            int ldvl,
            Complex* vr,
            int ldvr,
            int mm,
            int* m,
            int* ifaill,
            int* ifailr);
    }
}
