//-------------------------------
//BrightBloom_ps20.glsl
// High-pass filter for obtaining lumminance
// We use an aproximation formula that is pretty fast:
//   f(x) = ( -3 * ( x - 1 )^2 + 1 ) * 2
//   Color += Grayscale( f(Color) ) + 0.6
//
// Special thanks to ATI for their great HLSL2GLSL utility
//     http://sourceforge.net/projects/hlsl2glsl
//-------------------------------

uniform sampler2D RT;

void main()
{
    vec4 tex;
    vec4 bright4;
    float bright;
    
    vec2 texCoord = vec2( gl_TexCoord[0] );

    tex = texture2D( RT, texCoord);

    gl_FragColor = tex;
}
