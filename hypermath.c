#include <stdio.h>
#include <math.h>
#include <string.h>

typedef struct HPGmat4{
    float _11, _21, _31, _41,
	_12, _22, _32, _42,
	_13, _23, _33, _43,
	_14, _24, _34, _44;
} HPGmat4;

// Angle operations
float hpgDegreesToRadians(float deg){
    return deg * 0.0174532925;
}

float hpgRadiansToDegrees(float rad){
    return rad * 57.2957795;
}

// Vector operations
void hpgCross(float ax, float ay, float az, float bx, float by, float bz,
              float *rx, float *ry, float *rz){
    *rx = ay*bz - az*by;
    *ry = az*bx - ax*bz;
    *rz = ax*by - ay*bx;
}

void hpgNormalize(float x, float y, float z, float *rx, float *ry, float *rz){
    float len = sqrt(x*x + y*y + z*z);
    *rx = x / len;
    *ry = y / len;
    *rz = z / len;
}

float hpgDot(float ax, float ay, float az, float bx, float by, float bz){
    return ax*bx + ay*by + az*bz;
}

// Matrix operations
static void initMat4(HPGmat4 *m){
  memset(m, 0, sizeof(HPGmat4));
}

static void hpgCopyMat4(float *dest, const float *source){
    memcpy(dest, source, sizeof(float) * 16);
}

void hpgPrintMat4(const float *mat){
    HPGmat4 *m = (HPGmat4 *) mat;
    printf("[%f, %f, %f, %f,\n",   m->_11, m->_12, m->_13, m->_14);
    printf(" %f, %f, %f, %f,\n",   m->_21, m->_22, m->_23, m->_24);
    printf(" %f, %f, %f, %f,\n",   m->_31, m->_32, m->_33, m->_34);
    printf(" %f, %f, %f, %f]\n\n", m->_41, m->_42, m->_43, m->_44);
}

void hpgIdentityMat4(float *mat){
    HPGmat4 *m = (HPGmat4 *) mat;
    memset(m, 0, sizeof(HPGmat4));
    m->_11 = 1.0;
    m->_22 = 1.0;
    m->_33 = 1.0;
    m->_44 = 1.0;
}

void hpgMultMat4(const float *matA, const float *matB, float *result){
    HPGmat4 *a = (HPGmat4 *) matA;
    HPGmat4 *b = (HPGmat4 *) matB;
    HPGmat4 *r = (HPGmat4 *) result;
    r->_11 = a->_11*b->_11 + a->_12*b->_21 + a->_13*b->_31 + a->_14*b->_41;
    r->_12 = a->_11*b->_12 + a->_12*b->_22 + a->_13*b->_32 + a->_14*b->_42;
    r->_13 = a->_11*b->_13 + a->_12*b->_23 + a->_13*b->_33 + a->_14*b->_43;
    r->_14 = a->_11*b->_14 + a->_12*b->_24 + a->_13*b->_34 + a->_14*b->_44;

    r->_21 = a->_21*b->_11 + a->_22*b->_21 + a->_23*b->_31 + a->_24*b->_41;
    r->_22 = a->_21*b->_12 + a->_22*b->_22 + a->_23*b->_32 + a->_24*b->_42;
    r->_23 = a->_21*b->_13 + a->_22*b->_23 + a->_23*b->_33 + a->_24*b->_43;
    r->_24 = a->_21*b->_14 + a->_22*b->_24 + a->_23*b->_34 + a->_24*b->_44;

    r->_31 = a->_31*b->_11 + a->_32*b->_21 + a->_33*b->_31 + a->_34*b->_41;
    r->_32 = a->_31*b->_12 + a->_32*b->_22 + a->_33*b->_32 + a->_34*b->_42;
    r->_33 = a->_31*b->_13 + a->_32*b->_23 + a->_33*b->_33 + a->_34*b->_43;
    r->_34 = a->_31*b->_14 + a->_32*b->_24 + a->_33*b->_34 + a->_34*b->_44;

    r->_41 = a->_41*b->_11 + a->_42*b->_21 + a->_43*b->_31 + a->_44*b->_41;
    r->_42 = a->_41*b->_12 + a->_42*b->_22 + a->_43*b->_32 + a->_44*b->_42;
    r->_43 = a->_41*b->_13 + a->_42*b->_23 + a->_43*b->_33 + a->_44*b->_43;
    r->_44 = a->_41*b->_14 + a->_42*b->_24 + a->_43*b->_34 + a->_44*b->_44;
}

void hpgTranslate(float x, float y, float z, float *mat){
    HPGmat4 trans;
    float r[16];
    hpgIdentityMat4((float *) &trans);
    trans._14 += x;
    trans._24 += y;
    trans._34 += z;
    hpgMultMat4((float *) &trans, mat, r);
    hpgCopyMat4(mat, r);
}

void hpgRotateX(float rotation, float *mat){
    HPGmat4 rot;
    float r[16];
    initMat4(&rot);
    float c = cos(rotation);
    float s = sin(rotation);
    rot._11 = 1.0;
    rot._22 = c;
    rot._33 = c;
    rot._23 = -s;
    rot._32 = s;
    rot._44 = 1.0;
    hpgMultMat4((float *) &rot, mat, r);
    hpgCopyMat4(mat, r);
}

void hpgRotateY(float rotation, float *mat){
    HPGmat4 rot;
    float r[16];
    initMat4(&rot);
    float c = cos(rotation);
    float s = sin(rotation);
    rot._11 = c;
    rot._22 = 1.0;
    rot._33 = c;
    rot._13 = s;
    rot._31 = -s;
    rot._44 = 1.0;
    hpgMultMat4((float *) &rot, mat, r);
    hpgCopyMat4(mat, r);
}

void hpgRotateZ(float rotation, float *mat){
    HPGmat4 rot;
    float r[16];
    initMat4(&rot);
    float c = cos(rotation);
    float s = sin(rotation);
    rot._11 = c;
    rot._22 = c;
    rot._12 = -s;
    rot._21 = s;
    rot._33 = 1.0;
    rot._44 = 1.0;
    hpgMultMat4((float *) &rot, mat, r);
    hpgCopyMat4(mat, r);
}

void hpgRotate(float x, float y, float z, float angle, float *mat){
    HPGmat4 m;
    float r[16];
    float c = cos(angle);
    float s = sin(angle);
    float C = 1 - c;
    initMat4(&m);
    float xx, xy, xz, yy, yz, zz, xs, ys, zs;
    hpgNormalize(x, y, z, &x, &y, &z);
    xx = x * x;
    xy = x * y;
    xz = x * z;
    yy = y * y;
    yz = y * z;
    zz = z * z;
    xs = x * s;
    ys = y * s;
    zs = z * s;
    
    m._11 = xx*C + c;
    m._12 = xy*C - zs;
    m._13 = xz*C + ys;

    m._21 = xy*C + zs;
    m._22 = yy*C + c;
    m._23 = yz*C - xs;

    m._31 = xz*C - ys;
    m._32 = yz*C + xs;
    m._33 = zz*C + c;
    
    m._44 = 1.0;
    hpgMultMat4((float *) &m, mat, r);
    hpgCopyMat4(mat, r);
}

void hpgScale2D(float scaleX, float scaleY, float *mat){
    HPGmat4 scale;
    float r[16];
    initMat4(&scale);
    scale._11 = scaleX;
    scale._22 = scaleY;
    scale._33 = 1.0;
    scale._44 = 1.0;
    hpgMultMat4((float *) &scale, mat, r);
    hpgCopyMat4(mat, r);
}

void hpgScale3D(float scaleX, float scaleY, float scaleZ, float *mat){
    HPGmat4 scale;
    float r[16];
    initMat4(&scale);
    scale._11 = scaleX;
    scale._22 = scaleY;
    scale._33 = scaleZ;
    scale._44 = 1.0;
    hpgMultMat4((float *) &scale, mat, r);
    hpgCopyMat4(mat, r);
}

void hpgScale(float factor, float *mat){
    HPGmat4 scale;
    float r[16];
    initMat4(&scale);
    scale._11 = factor;
    scale._22 = factor;
    scale._33 = factor;
    scale._44 = 1.0;
    hpgMultMat4((float *) &scale, mat, r);
    hpgCopyMat4(mat, r);
}

void hpgFlipX(float *mat){
    HPGmat4 *m = (HPGmat4 *) mat;
    m->_22 = -m->_22;
}

void hpgFlipY(float *mat){
    HPGmat4 *m = (HPGmat4 *) mat;
    m->_11 = -m->_11;
}

void hpgFlipZ(float *mat){
    HPGmat4 *m = (HPGmat4 *) mat;
    m->_33 = -m->_33;
}

void hpgTranslateScale(float x, float y, float z, float scale, float *mat){
    HPGmat4 *m = (HPGmat4 *) mat;
    initMat4(m);
    m->_11 = scale;
    m->_22 = scale;
    m->_33 = scale;
    m->_44 = 1.0;
    m->_14 = x;
    m->_24 = y;
    m->_34 = z;
}

void hpgTranslateRotateScale2D(float x, float y, float z, float angle, float scale,
			        float *mat){
    HPGmat4 *m = (HPGmat4 *) mat;
    initMat4(m);
    float c = scale * cos(angle);
    float s = scale * sin(angle);
    m->_11 = c;
    m->_22 = c;
    m->_12 = -s;
    m->_21 = s;
    m->_33 = 1.0;
    m->_44 = 1.0;
    m->_14 = x;
    m->_24 = y;
    m->_34 = z;
}

void hpgTranslateRotateScale(float x, float y, float z, 
			     float qx, float qy, float qz, float qw,
                             float scale, float *mat){
    hpgTranslateScale(x, y, z, scale, mat);
    hpgRotate(qx, qy, qz, qw, mat);
}

void hpgTranspose(const float *mat, float *result){
    HPGmat4 *m = (HPGmat4 *) mat;
    HPGmat4 *r = (HPGmat4 *) result;
    r->_11 = m->_11;
    r->_12 = m->_21;
    r->_13 = m->_31;
    r->_14 = m->_41;

    r->_21 = m->_12;
    r->_22 = m->_22;
    r->_23 = m->_32;
    r->_24 = m->_42;

    r->_31 = m->_13;
    r->_32 = m->_23;
    r->_33 = m->_33;
    r->_34 = m->_43;

    r->_41 = m->_14;
    r->_42 = m->_24;
    r->_43 = m->_34;
    r->_44 = m->_44;
}

void hpgInverse(const float *mat, float *result){
    HPGmat4 *m = (HPGmat4 *) mat;
    HPGmat4 inv;
    float det;
    int i;

    inv._11 = m->_22  * m->_33 * m->_44 - 
        m->_22  * m->_34 * m->_43 - 
        m->_32  * m->_23  * m->_44 + 
        m->_32  * m->_24  * m->_43 +
        m->_42 * m->_23  * m->_34 - 
        m->_42 * m->_24  * m->_33;

    inv._21 = -m->_21  * m->_33 * m->_44 + 
        m->_21  * m->_34 * m->_43 + 
        m->_31  * m->_23  * m->_44 - 
        m->_31  * m->_24  * m->_43 - 
        m->_41 * m->_23  * m->_34 + 
        m->_41 * m->_24  * m->_33;

    inv._31 = m->_21  * m->_32 * m->_44 - 
        m->_21  * m->_34 * m->_42 - 
        m->_31  * m->_22 * m->_44 + 
        m->_31  * m->_24 * m->_42 + 
        m->_41 * m->_22 * m->_34 - 
        m->_41 * m->_24 * m->_32;

    inv._41 = -m->_21  * m->_32 * m->_43 + 
        m->_21  * m->_33 * m->_42 +
        m->_31  * m->_22 * m->_43 - 
        m->_31  * m->_23 * m->_42 - 
        m->_41 * m->_22 * m->_33 + 
        m->_41 * m->_23 * m->_32;

    inv._12 = -m->_12  * m->_33 * m->_44 + 
        m->_12  * m->_34 * m->_43 + 
        m->_32  * m->_13 * m->_44 - 
        m->_32  * m->_14 * m->_43 - 
        m->_42 * m->_13 * m->_34 + 
        m->_42 * m->_14 * m->_33;

    inv._22 = m->_11  * m->_33 * m->_44 - 
        m->_11  * m->_34 * m->_43 - 
        m->_31  * m->_13 * m->_44 + 
        m->_31  * m->_14 * m->_43 + 
        m->_41 * m->_13 * m->_34 - 
        m->_41 * m->_14 * m->_33;

    inv._32 = -m->_11  * m->_32 * m->_44 + 
        m->_11  * m->_34 * m->_42 + 
        m->_31  * m->_12 * m->_44 - 
        m->_31  * m->_14 * m->_42 - 
        m->_41 * m->_12 * m->_34 + 
        m->_41 * m->_14 * m->_32;

    inv._42 = m->_11  * m->_32 * m->_43 - 
        m->_11  * m->_33 * m->_42 - 
        m->_31  * m->_12 * m->_43 + 
        m->_31  * m->_13 * m->_42 + 
        m->_41 * m->_12 * m->_33 - 
        m->_41 * m->_13 * m->_32;

    inv._13 = m->_12  * m->_23 * m->_44 - 
        m->_12  * m->_24 * m->_43 - 
        m->_22  * m->_13 * m->_44 + 
        m->_22  * m->_14 * m->_43 + 
        m->_42 * m->_13 * m->_24 - 
        m->_42 * m->_14 * m->_23;

    inv._23 = -m->_11  * m->_23 * m->_44 + 
        m->_11  * m->_24 * m->_43 + 
        m->_21  * m->_13 * m->_44 - 
        m->_21  * m->_14 * m->_43 - 
        m->_41 * m->_13 * m->_24 + 
        m->_41 * m->_14 * m->_23;

    inv._33 = m->_11  * m->_22 * m->_44 - 
        m->_11  * m->_24 * m->_42 - 
        m->_21  * m->_12 * m->_44 + 
        m->_21  * m->_14 * m->_42 + 
        m->_41 * m->_12 * m->_24 - 
        m->_41 * m->_14 * m->_22;

    inv._43 = -m->_11  * m->_22 * m->_43 + 
        m->_11  * m->_23 * m->_42 + 
        m->_21  * m->_12 * m->_43 - 
        m->_21  * m->_13 * m->_42 - 
        m->_41 * m->_12 * m->_23 + 
        m->_41 * m->_13 * m->_22;

    inv._14 = -m->_12 * m->_23 * m->_34 + 
        m->_12 * m->_24 * m->_33 + 
        m->_22 * m->_13 * m->_34 - 
        m->_22 * m->_14 * m->_33 - 
        m->_32 * m->_13 * m->_24 + 
        m->_32 * m->_14 * m->_23;

    inv._24 = m->_11 * m->_23 * m->_34 - 
        m->_11 * m->_24 * m->_33 - 
        m->_21 * m->_13 * m->_34 + 
        m->_21 * m->_14 * m->_33 + 
        m->_31 * m->_13 * m->_24 - 
        m->_31 * m->_14 * m->_23;

    inv._34 = -m->_11 * m->_22 * m->_34 + 
        m->_11 * m->_24 * m->_32 + 
        m->_21 * m->_12 * m->_34 - 
        m->_21 * m->_14 * m->_32 - 
        m->_31 * m->_12 * m->_24 + 
        m->_31 * m->_14 * m->_22;

    inv._44 = m->_11 * m->_22 * m->_33 - 
        m->_11 * m->_23 * m->_32 - 
        m->_21 * m->_12 * m->_33 + 
        m->_21 * m->_13 * m->_32 + 
        m->_31 * m->_12 * m->_23 - 
        m->_31 * m->_13 * m->_22;

    det = m->_11 * inv._11 + m->_12 * inv._21 + m->_13 * inv._31 + m->_14 * inv._41;
    det = 1.0 / det;

    float *in = (float *) &inv;

    if (det == 0){
        for (i = 0; i < 16; i++)
            result[i] = 0;
    } else {
        for (i = 0; i < 16; i++)
            result[i] = in[i] * det;
    }
}

//
// Matrix creation
//

// Projection
void hpgOrtho(int width, int height, float near, float far, float *mat){
    HPGmat4 *m = (HPGmat4 *) mat;
    initMat4(m);
    m->_11 = 2.0 / (float) width;
    m->_22 = 2.0 / (float) height;
    m->_33 = -2 / (far - near);
    m->_34 = -(far + near) / (far - near);
    m->_44 = 1.0;
}

void hpgFrustum(float left, float right, float bottom, float top,
		float near, float far, float *mat){
    HPGmat4 *m = (HPGmat4 *) mat;
    initMat4(m);
    m->_11 = 2.0 * near / (right - left);
    m->_13 =  (right + left) / (right - left);
    m->_22 = 2.0 * near / (top - bottom);
    m->_23 = (top + bottom) / (top - bottom);
    m->_33 = -(far + near) / (far - near);
    m->_34 = - (2.0 * far * near) / (far - near);
    m->_43 = -1.0;
}

void hpgPerspective(int width, int height, float near, float far, float angle,
		    float *mat){
    float scale = tan(hpgDegreesToRadians(angle * 0.5)) * near;
    float r = ((float) width / (float) height) * scale;
    float l = -r;
    float t = scale;
    float b = -t;
    hpgFrustum(l, r, b, t, near, far, mat);
}

// Camera
void hpgLookAt(float eyeX, float eyeY, float eyeZ, float x, float y, float z, float upX, float upY, float upZ, float *mat){
    float fx, fy, fz, ux, uy, uz, rx, ry, rz;
    HPGmat4 *m = (HPGmat4 *) mat;
    initMat4(m);
    hpgNormalize(eyeX - x, eyeY - y, eyeZ - z, &fx, &fy, &fz);
    hpgCross(upX, upY, upZ, fx, fx, fz, &rx, &ry, &rz);
    hpgNormalize(rx, ry, rz, &rx, &ry, &rz);
    hpgCross(fx, fy, fz, rx, ry, rz, &ux, &uy, &uz);

    m->_11 = rx;
    m->_12 = ry;
    m->_13 = rz;
    m->_21 = ux;
    m->_22 = uy;
    m->_23 = uz;
    m->_31 = fx;
    m->_32 = fy;
    m->_33 = fz;
    m->_14 =-hpgDot(rx, ry, rz, eyeX, eyeY, eyeZ);
    m->_24 =-hpgDot(ux, uy, uz, eyeX, eyeY, eyeZ);
    m->_34 =-hpgDot(fx, fy, fz, eyeX, eyeY, eyeZ);
    m->_44 = 1.0;
}

/*
  http://ksimek.github.io/2012/08/22/extrinsic/
*/
void hpgCameraInverse(const float *camera, float *inverse){
    HPGmat4 *cam = (HPGmat4 *) camera;
    HPGmat4 *inv = (HPGmat4 *) inverse;
    // Rotation component
    inv->_11 = cam->_11;
    inv->_12 = cam->_21;
    inv->_13 = cam->_31;

    inv->_21 = cam->_12;
    inv->_22 = cam->_22;
    inv->_23 = cam->_32;

    inv->_31 = cam->_13;
    inv->_32 = cam->_23;
    inv->_33 = cam->_33;
    // Translation component
    inv->_14 = -(cam->_11*cam->_14 + cam->_21*cam->_24 + cam->_31*cam->_34);
    inv->_24 = -(cam->_12*cam->_14 + cam->_22*cam->_24 + cam->_32*cam->_34);
    inv->_34 = -(cam->_13*cam->_14 + cam->_23*cam->_24 + cam->_33*cam->_34);
    // Last row
    inv->_41 = 0;
    inv->_42 = 0;
    inv->_43 = 0;
    inv->_44 = 1;
}
