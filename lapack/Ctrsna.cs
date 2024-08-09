using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// CTRSNA estimates reciprocal condition numbers for specified
        /// eigenvalues and/or right eigenvectors of a complex upper triangular
        /// matrix T (or of any matrix Q*T*Q**H with Q unitary).
        /// </para>
        /// </summary>
        /// <param name="matrixLayout">
        /// Specifies the matrix layout.
        /// </param>
        /// <param name="job">
        /// [in] JOB is CHARACTER*1.
        /// Specifies whether condition numbers are required for
        /// eigenvalues (S) or eigenvectors (SEP):
        /// = &#39;E&#39;: for eigenvalues only (S);
        /// = &#39;V&#39;: for eigenvectors only (SEP);
        /// = &#39;B&#39;: for both eigenvalues and eigenvectors (S and SEP).
        /// </param>
        /// <param name="howmny">
        /// [in] HOWMNY is CHARACTER*1.
        /// = &#39;A&#39;: compute condition numbers for all eigenpairs;
        /// = &#39;S&#39;: compute condition numbers for selected eigenpairs
        /// specified by the array SELECT.
        /// </param>
        /// <param name="select">
        /// [in] SELECT is LOGICAL array, dimension (N).
        /// If HOWMNY = &#39;S&#39;, SELECT specifies the eigenpairs for which
        /// condition numbers are required. To select condition numbers
        /// for the j-th eigenpair, SELECT(j) must be set to .TRUE..
        /// If HOWMNY = &#39;A&#39;, SELECT is not referenced.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix T. N &gt;= 0.
        /// </param>
        /// <param name="t">
        /// [in] T is COMPLEX array, dimension (LDT,N).
        /// The upper triangular matrix T.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T. LDT &gt;= max(1,N).
        /// </param>
        /// <param name="vl">
        /// [in] VL is COMPLEX array, dimension (LDVL,M).
        /// If JOB = &#39;E&#39; or &#39;B&#39;, VL must contain left eigenvectors of T
        /// (or of any Q*T*Q**H with Q unitary), corresponding to the
        /// eigenpairs specified by HOWMNY and SELECT. The eigenvectors
        /// must be stored in consecutive columns of VL, as returned by
        /// CHSEIN or CTREVC.
        /// If JOB = &#39;V&#39;, VL is not referenced.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the array VL.
        /// LDVL &gt;= 1; and if JOB = &#39;E&#39; or &#39;B&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [in] VR is COMPLEX array, dimension (LDVR,M).
        /// If JOB = &#39;E&#39; or &#39;B&#39;, VR must contain right eigenvectors of T
        /// (or of any Q*T*Q**H with Q unitary), corresponding to the
        /// eigenpairs specified by HOWMNY and SELECT. The eigenvectors
        /// must be stored in consecutive columns of VR, as returned by
        /// CHSEIN or CTREVC.
        /// If JOB = &#39;V&#39;, VR is not referenced.
        /// </param>
        /// <param name="ldvr">
        /// [in] LDVR is INTEGER.
        /// The leading dimension of the array VR.
        /// LDVR &gt;= 1; and if JOB = &#39;E&#39; or &#39;B&#39;, LDVR &gt;= N.
        /// </param>
        /// <param name="s">
        /// [out] S is REAL array, dimension (MM).
        /// If JOB = &#39;E&#39; or &#39;B&#39;, the reciprocal condition numbers of the
        /// selected eigenvalues, stored in consecutive elements of the
        /// array. Thus S(j), SEP(j), and the j-th columns of VL and VR
        /// all correspond to the same eigenpair (but not in general the
        /// j-th eigenpair, unless all eigenpairs are selected).
        /// If JOB = &#39;V&#39;, S is not referenced.
        /// </param>
        /// <param name="sep">
        /// [out] SEP is REAL array, dimension (MM).
        /// If JOB = &#39;V&#39; or &#39;B&#39;, the estimated reciprocal condition
        /// numbers of the selected eigenvectors, stored in consecutive
        /// elements of the array.
        /// If JOB = &#39;E&#39;, SEP is not referenced.
        /// </param>
        /// <param name="mm">
        /// [in] MM is INTEGER.
        /// The number of elements in the arrays S (if JOB = &#39;E&#39; or &#39;B&#39;)
        /// and/or SEP (if JOB = &#39;V&#39; or &#39;B&#39;). MM &gt;= M.
        /// </param>
        /// <param name="m">
        /// [out] M is INTEGER.
        /// The number of elements of the arrays S and/or SEP actually
        /// used to store the estimated condition numbers.
        /// If HOWMNY = &#39;A&#39;, M is set to N.
        /// </param>
        /// <returns>
        /// = 0: successful exit
        /// &lt; 0: if INFO = -i, the i-th argument had an illegal value
        /// </returns>
        /// <remarks>
        /// <para>
        ///  The reciprocal of the condition number of an eigenvalue lambda is
        ///  defined as
        /// </para>
        /// <para>
        ///          S(lambda) = |v**H*u| / (norm(u)*norm(v))
        /// </para>
        /// <para>
        ///  where u and v are the right and left eigenvectors of T corresponding
        ///  to lambda; v**H denotes the conjugate transpose of v, and norm(u)
        ///  denotes the Euclidean norm. These reciprocal condition numbers always
        ///  lie between zero (very badly conditioned) and one (very well
        ///  conditioned). If n = 1, S(lambda) is defined to be 1.
        /// </para>
        /// <para>
        ///  An approximate error bound for a computed eigenvalue W(i) is given by
        /// </para>
        /// <para>
        ///                      EPS * norm(T) / S(i)
        /// </para>
        /// <para>
        ///  where EPS is the machine precision.
        /// </para>
        /// <para>
        ///  The reciprocal of the condition number of the right eigenvector u
        ///  corresponding to lambda is defined as follows. Suppose
        /// </para>
        /// <para>
        ///              T = ( lambda  c  )
        ///                  (   0    T22 )
        /// </para>
        /// <para>
        ///  Then the reciprocal condition number is
        /// </para>
        /// <para>
        ///          SEP( lambda, T22 ) = sigma-min( T22 - lambda*I )
        /// </para>
        /// <para>
        ///  where sigma-min denotes the smallest singular value. We approximate
        ///  the smallest singular value by the reciprocal of an estimate of the
        ///  one-norm of the inverse of T22 - lambda*I. If n = 1, SEP(1) is
        ///  defined to be abs(T(1,1)).
        /// </para>
        /// <para>
        ///  An approximate error bound for a computed right eigenvector VR(i)
        ///  is given by
        /// </para>
        /// <para>
        ///                      EPS * norm(T) / SEP(i)
        /// </para>
        /// </remarks>
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_ctrsna", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Ctrsna(
            MatrixLayout matrixLayout,
            char job,
            char howmny,
            bool* select,
            int n,
            Complex32* t,
            int ldt,
            Complex32* vl,
            int ldvl,
            Complex32* vr,
            int ldvr,
            float* s,
            float* sep,
            int mm,
            int* m);
    }
}
