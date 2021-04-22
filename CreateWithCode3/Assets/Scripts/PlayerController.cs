using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerController : MonoBehaviour
{
    private Rigidbody playerRb;
    private Animator playerAnimator;
    private AudioSource playerAudio;

    public ParticleSystem explosionParticle;
    public ParticleSystem splatterParticle;
    public AudioClip crashSound;
    public AudioClip jumpSound;
    public float gravityModifier;
    public float jumpForce;
    public bool isOnGround = true;
    public bool gameOver = false;

    // Start is called before the first frame update
    void Start()
    {
        playerRb = GetComponent<Rigidbody>();
        playerAnimator = GetComponent<Animator>();
        playerAudio = GetComponent<AudioSource>();
        Physics.gravity *= gravityModifier;
    }

    // Update is called once per frame
    void Update()
    {
        if(Input.GetKeyDown(KeyCode.Space) && isOnGround && !gameOver)
        {
            playerRb.AddForce(Vector3.up * jumpForce);
            isOnGround = false;
            playerAnimator.SetTrigger("Jump_trig");
            splatterParticle.Stop();
            playerAudio.PlayOneShot(jumpSound, 1.0f);
        }
        
    }

    private void OnCollisionEnter(Collision collision)
    {
        if (collision.gameObject.CompareTag("Ground")) {
            splatterParticle.Play();
            isOnGround = true;
        }
        else if(collision.gameObject.CompareTag("Obstacle")) {
            gameOver = true;
            Debug.Log("Game Over!!!!!!!!");
            playerAnimator.SetBool("Death_b", true);
            playerAnimator.SetInteger("DeathType_int", 1);
            explosionParticle.Play();
            splatterParticle.Stop();
            playerAudio.PlayOneShot(crashSound, 0.5f);
        }
    }   
}
