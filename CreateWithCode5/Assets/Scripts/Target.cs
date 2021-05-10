using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Target : MonoBehaviour
{
    public int value;
    public ParticleSystem particle;
    private Rigidbody targetRb;
    private GameManager gameManager;
    private float minSpeed = 13;
    private float maxSpeed = 17; 
    private float maxTorque = 10;
    private float xRange = 4;
    private float y = -6;
    // Start is called before the first frame update
    void Start()
    {
        gameManager = GameObject.Find("GameManager").GetComponent<GameManager>();
        targetRb = GetComponent<Rigidbody>();
        targetRb.AddForce(RandomForce(), ForceMode.Impulse);
        targetRb.AddTorque(RandomTorque(), RandomTorque(), RandomTorque());
        transform.position = RandomSpawnPos();
    }

    private void OnMouseDown()
    {
        if (!gameManager.IsGameOver)
        {
            if (gameObject.CompareTag("BadProps"))
            {
                gameManager.GameOver();
            }

            Destroy(gameObject);
            Instantiate(particle, transform.position, particle.transform.rotation);

            gameManager.updateScore(value);
        }

    }

    private void OnTriggerEnter(Collider other)
    {
        Destroy(gameObject);
    }

    float RandomTorque() {
        return Random.Range(-maxTorque, maxTorque);
    }
    Vector3 RandomForce() {
        return Vector3.up * Random.Range(minSpeed, maxSpeed);
    }
    Vector3 RandomSpawnPos() {
        return new Vector3(Random.Range(-xRange, xRange), y);
    }

    // Update is called once per frame
    void Update()
    {
        
    }


}
