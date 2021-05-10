using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PlayerController : MonoBehaviour
{
    // Start is called before the first frame update
    public float speed;
    private GameObject camera;
    private Rigidbody playerRb;
    private bool hasPower;
    public GameObject powerUpIndicator;
    void Start()
    {
        camera = GameObject.Find("FocalPoint");
        playerRb = GetComponent<Rigidbody>();
    }

    // Update is called once per frame
    void Update()
    {
        float vertical = Input.GetAxis("Vertical");
        playerRb.AddForce(camera.transform.forward * vertical * speed);
        powerUpIndicator.transform.position = transform.position + new Vector3(0, -0.5f, 0);
    }

    private void OnTriggerEnter(Collider other)
    {
        if(other.CompareTag("PowerUp"))
        {
            hasPower = true;
            Destroy(other.gameObject);
            Debug.Log("Player get power !!!!!!!!!!!!!!!!!!");
            powerUpIndicator.SetActive(true);
        }
    }

    IEnumerator PowerUpcountDown() {
        yield return new WaitForSeconds(7);
        hasPower = false;
        powerUpIndicator.SetActive(false);
    }
    private void OnCollisionEnter(Collision collision)
    {
        float powerUpStrength = 50.0f;

        if(collision.gameObject.CompareTag("Enemy") && hasPower)
        {
            GameObject obj = collision.gameObject;
            Rigidbody enemyRb = obj.GetComponent<Rigidbody>();
            Vector3 awayFromPlayer = (obj.transform.position - transform.position).normalized;
            enemyRb.AddForce(awayFromPlayer * powerUpStrength, ForceMode.Impulse);
            StartCoroutine(PowerUpcountDown());
        }
    }
}
