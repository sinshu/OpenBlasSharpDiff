﻿using System;
using System.Numerics;
using System.Runtime.InteropServices;

namespace OpenBlasSharp
{
    public static partial class Lapack
    {
        /// <summary>
        /// <para>
        /// STRSNA estimates reciprocal condition numbers for specified
        /// eigenvalues and/or right eigenvectors of a real upper
        /// quasi-triangular matrix T (or of any matrix Q*T*Q**T with Q
        /// orthogonal).
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
        /// for the eigenpair corresponding to a real eigenvalue w(j),
        /// SELECT(j) must be set to .TRUE.. To select condition numbers
        /// corresponding to a complex conjugate pair of eigenvalues w(j)
        /// and w(j+1), either SELECT(j) or SELECT(j+1) or both, must be
        /// set to .TRUE..
        /// If HOWMNY = &#39;A&#39;, SELECT is not referenced.
        /// </param>
        /// <param name="n">
        /// [in] N is INTEGER.
        /// The order of the matrix T. N &gt;= 0.
        /// </param>
        /// <param name="t">
        /// [in] T is REAL array, dimension (LDT,N).
        /// The upper quasi-triangular matrix T, in Schur canonical form.
        /// </param>
        /// <param name="ldt">
        /// [in] LDT is INTEGER.
        /// The leading dimension of the array T. LDT &gt;= max(1,N).
        /// </param>
        /// <param name="vl">
        /// [in] VL is REAL array, dimension (LDVL,M).
        /// If JOB = &#39;E&#39; or &#39;B&#39;, VL must contain left eigenvectors of T
        /// (or of any Q*T*Q**T with Q orthogonal), corresponding to the
        /// eigenpairs specified by HOWMNY and SELECT. The eigenvectors
        /// must be stored in consecutive columns of VL, as returned by
        /// SHSEIN or STREVC.
        /// If JOB = &#39;V&#39;, VL is not referenced.
        /// </param>
        /// <param name="ldvl">
        /// [in] LDVL is INTEGER.
        /// The leading dimension of the array VL.
        /// LDVL &gt;= 1; and if JOB = &#39;E&#39; or &#39;B&#39;, LDVL &gt;= N.
        /// </param>
        /// <param name="vr">
        /// [in] VR is REAL array, dimension (LDVR,M).
        /// If JOB = &#39;E&#39; or &#39;B&#39;, VR must contain right eigenvectors of T
        /// (or of any Q*T*Q**T with Q orthogonal), corresponding to the
        /// eigenpairs specified by HOWMNY and SELECT. The eigenvectors
        /// must be stored in consecutive columns of VR, as returned by
        /// SHSEIN or STREVC.
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
        /// array. For a complex conjugate pair of eigenvalues two
        /// consecutive elements of S are set to the same value. Thus
        /// S(j), SEP(j), and the j-th columns of VL and VR all
        /// correspond to the same eigenpair (but not in general the
        /// j-th eigenpair, unless all eigenpairs are selected).
        /// If JOB = &#39;V&#39;, S is not referenced.
        /// </param>
        /// <param name="sep">
        /// [out] SEP is REAL array, dimension (MM).
        /// If JOB = &#39;V&#39; or &#39;B&#39;, the estimated reciprocal condition
        /// numbers of the selected eigenvectors, stored in consecutive
        /// elements of the array. For a complex eigenvector two
        /// consecutive elements of SEP are set to the same value. If
        /// the eigenvalues cannot be reordered to compute SEP(j), SEP(j)
        /// is set to 0; this can only occur when the true value would be
        /// very small anyway.
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
        ///          S(lambda) = |v**T*u| / (norm(u)*norm(v))
        /// </para>
        /// <para>
        ///  where u and v are the right and left eigenvectors of T corresponding
        ///  to lambda; v**T denotes the transpose of v, and norm(u)
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
        [DllImport(OpenBlas.LibraryName, EntryPoint = "LAPACKE_strsna", CallingConvention = CallingConvention.Cdecl)]
        public static extern unsafe LapackInfo Strsna(
            MatrixLayout matrixLayout,
            char job,
            char howmny,
            bool* select,
            int n,
            float* t,
            int ldt,
            float* vl,
            int ldvl,
            float* vr,
            int ldvr,
            float* s,
            float* sep,
            int mm,
            int* m);
    }
}
