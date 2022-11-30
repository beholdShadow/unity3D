using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Move : MonoBehaviour
{
    public float startSpeed;
    public float acceleration;
    public GameObject impact;
    private float curSpeed;
    private Rigidbody rb;
    public List<GameObject> trails;
    // Start is called before the first frame update
    void Start()
    {
        rb = GetComponent<Rigidbody>();
        curSpeed = startSpeed;
    }

    // Update is called once per frame
    void Update()
    {
        if(rb != null)
        {
            curSpeed = curSpeed + acceleration * Time.deltaTime;
            rb.position += transform.forward * curSpeed * Time.deltaTime;
 
        }
    }

    private void OnCollisionEnter(Collision collision)
    {
        ContactPoint contact = collision.contacts[0];
        Quaternion rot = Quaternion.FromToRotation(Vector3.up, contact.normal);
        Vector3 pos = contact.point;

        if(impact)
        {
            var vfx = Instantiate(impact, pos, rot) as GameObject;
            Destroy(vfx, 3);
        }

        if(trails.Count > 0)
        {
            for(int i = 0; i < trails.Count; i++)
            {
                trails[i].transform.parent = null;
                var ps = trails[i].GetComponent<ParticleSystem>();
                if (ps)
                {
                    ps.Stop();
                    Destroy(ps.gameObject, ps.main.duration + ps.main.startLifetime.constantMax);
                }
            }
        }
        Destroy(gameObject);
    }

}
